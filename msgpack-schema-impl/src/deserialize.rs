use crate::attr;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Error, Fields, FieldsNamed, FieldsUnnamed, Result,
};

enum FieldKind {
    Ordinary(u32),
    Optional(u32),
    Flatten,
    Skip,
}
pub fn derive(node: &DeriveInput) -> Result<TokenStream> {
    let attrs = attr::get(&node.attrs)?;
    attrs.disallow_optional()?;
    attrs.disallow_tag()?;
    attrs.disallow_flatten()?;
    match &node.data {
        Data::Struct(strut) => match &strut.fields {
            Fields::Named(fields) => {
                if attrs.untagged.is_some() {
                    derive_untagged_struct(node, strut, fields)
                } else {
                    derive_struct(node, strut, fields)
                }
            }
            Fields::Unnamed(fields) => {
                attrs.disallow_untagged()?;
                let len = fields.unnamed.len();
                match len {
                    0 => Err(Error::new_spanned(
                        node,
                        "empty tuple structs as deserialize are not supported",
                    )),
                    1 => derive_newtype_struct(node, strut, &fields.unnamed[0]),
                    _ => derive_tuple_struct(node, strut, fields),
                }
            }
            Fields::Unit => {
                attrs.disallow_untagged()?;
                Err(Error::new_spanned(
                    node,
                    "unit structs as deserialize are not supported",
                ))
            }
        },
        Data::Enum(enu) => {
            if attrs.untagged.is_some() {
                derive_untagged_enum(node, enu)
            } else {
                derive_enum(node, enu)
            }
        }
        Data::Union(_) => Err(Error::new_spanned(
            node,
            "union as deserialize are not supported",
        )),
    }
}

fn derive_struct(
    node: &DeriveInput,
    _strut: &DataStruct,
    named_fields: &FieldsNamed,
) -> Result<TokenStream> {
    let ty = &node.ident;
    let (impl_generics, ty_generics, where_clause) = node.generics.split_for_impl();

    let fields = process_fields(named_fields)?;
    let field_initializers = generate_field_initializers(&fields);
    let tag_handlers = generate_tag_handlers(&fields);
    let field_constructors = generate_field_constructors(&fields);

    let fn_body = quote! {
        #field_initializers

        let __len = match __deserializer.deserialize_token()? {
            ::msgpack_schema::Token::Map(len) => len,
            _ => return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("expected a map").into())),
        };
        for _ in 0..__len {
            let __tag: u32 = __deserializer.deserialize()?;
            match __tag {
                #tag_handlers
                _ => {
                    __deserializer.deserialize_any()?;
                }
            }
        }
        Ok(Self {
            #field_constructors
        })
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Deserialize for #ty #ty_generics #where_clause {
            fn deserialize(__deserializer: &mut ::msgpack_schema::Deserializer) -> ::std::result::Result<Self, ::msgpack_schema::DeserializeError> {
                #fn_body
            }
        }
    };

    Ok(gen)
}

fn derive_newtype_struct(
    node: &DeriveInput,
    _strut: &DataStruct,
    field: &syn::Field,
) -> Result<TokenStream> {
    let ty = &node.ident;
    let (impl_generics, ty_generics, where_clause) = node.generics.split_for_impl();

    let attrs = attr::get(&field.attrs)?;
    attrs.disallow_tag()?;
    attrs.disallow_optional()?;
    attrs.disallow_untagged()?;
    attrs.disallow_flatten()?;

    let fn_body = quote! {
        __deserializer.deserialize().map(Self)
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Deserialize for #ty #ty_generics #where_clause {
            fn deserialize(__deserializer: &mut ::msgpack_schema::Deserializer) -> ::std::result::Result<Self, ::msgpack_schema::DeserializeError> {
                #fn_body
            }
        }
    };

    Ok(gen)
}

fn derive_tuple_struct(
    node: &DeriveInput,
    _strut: &DataStruct,
    fields: &FieldsUnnamed,
) -> Result<TokenStream> {
    let ty = &node.ident;
    let (impl_generics, ty_generics, where_clause) = node.generics.split_for_impl();

    for field in &fields.unnamed {
        let attrs = attr::get(&field.attrs)?;
        attrs.disallow_tag()?;
        attrs.disallow_optional()?;
        attrs.disallow_untagged()?;
        attrs.disallow_flatten()?;
    }

    let count = fields.unnamed.len() as u32;

    let members = (0..count).map(|_| {
        quote! {
            __deserializer.deserialize()?
        }
    });

    let fn_body = quote! {
        match __deserializer.deserialize_token()? {
            ::msgpack_schema::Token::Array(len) => {
                if len != #count {
                    return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("unexpected len").into()))
                }
            },
            _ => return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("not an array".into()))),
        };

        Ok(Self(
            #( #members ),*
        ))
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Deserialize for #ty #ty_generics #where_clause {
            fn deserialize(__deserializer: &mut ::msgpack_schema::Deserializer) -> ::std::result::Result<Self, ::msgpack_schema::DeserializeError> {
                #fn_body
            }
        }
    };

    Ok(gen)
}

fn derive_enum(node: &DeriveInput, enu: &DataEnum) -> Result<TokenStream> {
    let ty = &node.ident;
    let (impl_generics, ty_generics, where_clause) = node.generics.split_for_impl();

    let fn_body = {
        let mut clauses = vec![];
        let mut tags = vec![];
        for variant in &enu.variants {
            let ident = variant.ident.clone();
            let attrs = attr::get(&variant.attrs)?;
            attrs.disallow_optional()?;
            attrs.disallow_untagged()?;
            attrs.disallow_flatten()?;
            attrs.require_tag(variant)?;
            attr::check_tag_uniqueness(attrs.tag.as_ref().unwrap(), &mut tags)?;
            let tag = attrs.tag.unwrap().tag;
            match &variant.fields {
                Fields::Named(fields) => {
                    // Process the fields using common helper
                    let variant_fields = process_fields(fields)?;

                    // Generate field initializers
                    let field_inits = generate_field_initializers(&variant_fields);

                    // Generate tag handlers
                    let tag_handlers = generate_tag_handlers(&variant_fields);

                    // Generate field constructors
                    let field_ctors = generate_field_constructors(&variant_fields);

                    // Add the clause for this named-fields variant
                    clauses.push(quote! {
                        #tag => {
                            if !__is_array {
                                return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("not an array".into())));
                            }

                            // Initialize fields
                            #field_inits

                            // Deserialize map entries
                            let __len = match __deserializer.deserialize_token()? {
                                ::msgpack_schema::Token::Map(len) => len,
                                _ => return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("not a map".into()))),
                            };

                            for _ in 0..__len {
                                let __tag: u32 = __deserializer.deserialize()?;
                                match __tag {
                                    #tag_handlers
                                    _ => {
                                        __deserializer.deserialize_any()?;
                                    }
                                }
                            }

                            Ok(Self::#ident {
                                #field_ctors
                            })
                        }
                    });
                }
                Fields::Unnamed(fields) => {
                    let len = fields.unnamed.len() as u32;
                    match len {
                        0 => {
                            clauses.push(quote! {
                                #tag => {
                                    if __is_array {
                                        return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("not an array".into())));
                                    }
                                    Ok(Self::#ident())
                                }
                            });
                        }
                        1 => {
                            let attrs = attr::get(&fields.unnamed[0].attrs)?;
                            attrs.disallow_optional()?;
                            attrs.disallow_tag()?;
                            attrs.disallow_untagged()?;
                            attrs.disallow_flatten()?;
                            clauses.push(quote! {
                                #tag => {
                                    if !__is_array {
                                        return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("not an array".into())));
                                    }
                                    Ok(Self::#ident(__deserializer.deserialize()?))
                                }
                            });
                        }
                        _ => {
                            return Err(Error::new_spanned(
                                node,
                                "tuple variants with more than one elements are not supported",
                            ));
                        }
                    }
                }
                Fields::Unit => {
                    clauses.push(quote! {
                        #tag => {
                            Ok(Self::#ident)
                        }
                    });
                }
            }
        }

        quote! {
            let (__tag, __is_array): (u32, bool) = match __deserializer.deserialize_token()? {
                ::msgpack_schema::Token::Int(v) => {
                    (<u32 as ::std::convert::TryFrom<_>>::try_from(v).map_err(|_| ::msgpack_schema::DeserializeError::Validation(ValidationError("invalid integer".into())))?, false)
                }
                ::msgpack_schema::Token::Array(len) => {
                    if len != 2 {
                        return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("expected array of length 2".into())));
                    }
                    (__deserializer.deserialize::<u32>()?, true)
                }
                _ => {
                    return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("invalid token".into())));
                }
            };
            match __tag {
                #( #clauses )*
                _ => Err(::msgpack_schema::DeserializeError::Validation(ValidationError("invalid tag".into()))),
            }
        }
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Deserialize for #ty #ty_generics #where_clause {
            fn deserialize(__deserializer: &mut ::msgpack_schema::Deserializer) -> ::std::result::Result<Self, ::msgpack_schema::DeserializeError> {
                #fn_body
            }
        }
    };

    Ok(gen)
}

fn derive_untagged_enum(node: &DeriveInput, enu: &DataEnum) -> Result<TokenStream> {
    let ty = &node.ident;
    let (impl_generics, ty_generics, where_clause) = node.generics.split_for_impl();

    let fn_body = {
        let mut members = vec![];
        for variant in &enu.variants {
            let attrs = attr::get(&variant.attrs)?;
            attrs.disallow_optional()?;
            attrs.disallow_tag()?;
            attrs.disallow_untagged()?;
            attrs.disallow_flatten()?;
            match &variant.fields {
                Fields::Named(_) => {
                    return Err(Error::new_spanned(
                        node,
                        "struct variants cannot be untagged",
                    ));
                }
                Fields::Unnamed(fields) => match fields.unnamed.len() {
                    0 => {
                        return Err(Error::new_spanned(
                            node,
                            "empty tuple variants cannot be untagged",
                        ));
                    }
                    1 => {
                        let attrs = attr::get(&fields.unnamed[0].attrs)?;
                        attrs.disallow_optional()?;
                        attrs.disallow_tag()?;
                        attrs.disallow_untagged()?;
                        attrs.disallow_flatten()?;
                        members.push((variant, &fields.unnamed[0]));
                    }
                    _ => {
                        return Err(Error::new_spanned(
                            node,
                            "tuple variants cannot be untagged",
                        ));
                    }
                },
                Fields::Unit => {
                    return Err(Error::new_spanned(
                        node,
                        "unit variants cannot be supported",
                    ));
                }
            }
        }

        let mut clauses = vec![];
        for (variant, field) in &members {
            let ident = variant.ident.clone();
            let ty = field.ty.clone();
            clauses.push(quote! {
                if let Some(x) = __deserializer.try_deserialize::<#ty>()? {
                    return Ok(Self::#ident(x));
                }
            })
        }

        quote! {
            #( #clauses )*
            Err(::msgpack_schema::DeserializeError::Validation(ValidationError("invalid enum variant".into())))
        }
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Deserialize for #ty #ty_generics #where_clause {
            fn deserialize(__deserializer: &mut ::msgpack_schema::Deserializer) -> ::std::result::Result<Self, ::msgpack_schema::DeserializeError> {
                #fn_body
            }
        }
    };

    Ok(gen)
}

fn derive_untagged_struct(
    node: &DeriveInput,
    _strut: &DataStruct,
    named_fields: &FieldsNamed,
) -> Result<TokenStream> {
    let ty = &node.ident;
    let (impl_generics, ty_generics, where_clause) = node.generics.split_for_impl();

    let fn_body = {
        let mut members = vec![];
        for field in &named_fields.named {
            let attrs = attr::get(&field.attrs)?;
            attrs.disallow_tag()?;
            attrs.disallow_optional()?;
            attrs.disallow_untagged()?;
            attrs.disallow_flatten()?;
            let ident = field.ident.clone().unwrap();
            let ty = field.ty.clone();
            members.push((ident, ty))
        }

        let len = members.len() as u32;

        let mut init = vec![];
        for (ident, ty) in &members {
            let push = quote! {
                let mut #ident: #ty = __deserializer.deserialize()?;
            };
            init.push(push);
        }

        let mut ctors = vec![];
        for (ident, _ty) in &members {
            let push = quote! {
                #ident,
            };
            ctors.push(push);
        }

        quote! {
            let __len = match __deserializer.deserialize_token()? {
                Token::Array(len) => len,
                _ => return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("expected array".into()))),
            };

            if __len != #len {
                return Err(::msgpack_schema::DeserializeError::Validation(ValidationError("invalid length".into())));
            }
            #( #init )*
            Ok(Self {
                #( #ctors )*
            })
        }
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Deserialize for #ty #ty_generics #where_clause {
            fn deserialize(__deserializer: &mut ::msgpack_schema::Deserializer) -> ::std::result::Result<Self, ::msgpack_schema::DeserializeError> {
                #fn_body
            }
        }
    };

    Ok(gen)
}

/// Processes fields to extract field kind information
#[allow(clippy::type_complexity)]
fn process_fields(
    fields: &FieldsNamed,
) -> Result<Vec<(proc_macro2::Ident, syn::Type, FieldKind, Option<String>)>> {
    let mut processed_fields = vec![];
    let mut tags = vec![];

    for field in &fields.named {
        let ident = field.ident.clone().unwrap();
        let ty = field.ty.clone();
        let attrs = attr::get(&field.attrs)?;
        attrs.disallow_untagged()?;

        // Extract default function path if any
        let default_fn = attrs.default.as_ref().map(|d| d.path.clone());

        let kind = if attrs.skip.is_some() {
            // Skip attribute takes precedence
            attrs.disallow_tag()?;
            attrs.disallow_optional()?;
            attrs.disallow_flatten()?;
            FieldKind::Skip
        } else if attrs.flatten.is_some() {
            attrs.disallow_tag()?;
            attrs.disallow_optional()?;
            FieldKind::Flatten
        } else {
            attrs.require_tag(field)?;
            attr::check_tag_uniqueness(attrs.tag.as_ref().unwrap(), &mut tags)?;
            let tag = attrs.tag.unwrap().tag;
            // TODO: require `#[required]` or `#[optional]` for fields of the Option<T> type
            if attrs.optional.is_some() {
                FieldKind::Optional(tag)
            } else {
                FieldKind::Ordinary(tag)
            }
        };

        processed_fields.push((ident, ty, kind, default_fn));
    }

    Ok(processed_fields)
}

/// Generates field initialization code based on field kinds
fn generate_field_initializers(
    fields: &Vec<(proc_macro2::Ident, syn::Type, FieldKind, Option<String>)>,
) -> TokenStream {
    let mut initializers = vec![];

    for (ident, ty, kind, _default_fn) in fields {
        let code = match kind {
            FieldKind::Ordinary(_) => {
                quote! {
                    let mut #ident: ::std::option::Option<#ty> = None;
                }
            }
            FieldKind::Optional(_) => {
                quote! {
                    let mut #ident: #ty = None;
                }
            }
            FieldKind::Flatten => {
                quote! {
                    let #ident: #ty = __deserializer.clone().deserialize()?;
                }
            }
            FieldKind::Skip => {
                quote! {
                    let #ident: #ty = Default::default();
                }
            }
        };
        initializers.push(code);
    }

    quote! {
        #( #initializers )*
    }
}

/// Generates tag handlers for field deserialization
fn generate_tag_handlers(
    fields: &Vec<(proc_macro2::Ident, syn::Type, FieldKind, Option<String>)>,
) -> TokenStream {
    let mut handlers = vec![];

    for (ident, _, kind, _default_fn) in fields {
        match kind {
            FieldKind::Ordinary(tag) | FieldKind::Optional(tag) => {
                handlers.push(quote! {
                    #tag => {
                        match __deserializer.deserialize() {
                            Ok(__value) => {
                                #ident = Some(__value);
                            }
                            Err(::msgpack_schema::DeserializeError::Validation(_)) => {
                                #ident = None;
                            }
                            Err(e @ ::msgpack_schema::DeserializeError::InvalidInput(_)) => {
                                return Err(e);
                            }
                        }
                    }
                });
            }
            FieldKind::Flatten => {}
            FieldKind::Skip => {}
        }
    }

    quote! {
        #( #handlers )*
    }
}

/// Generates field constructors for the final object
fn generate_field_constructors(
    fields: &Vec<(proc_macro2::Ident, syn::Type, FieldKind, Option<String>)>,
) -> TokenStream {
    let mut constructors = vec![];

    for (ident, _, kind, default_fn) in fields {
        let code = match kind {
            FieldKind::Ordinary(_) => {
                if let Some(default_path) = default_fn {
                    let path = syn::parse_str::<syn::Path>(default_path).unwrap();
                    quote! {
                        #ident: #ident.unwrap_or_else(|| #path()),
                    }
                } else {
                    quote! {
                        #ident: #ident.ok_or(::msgpack_schema::DeserializeError::Validation(ValidationError("invalid ident").into()))?,
                    }
                }
            }
            FieldKind::Optional(_) | FieldKind::Flatten => {
                quote! {
                    #ident,
                }
            }
            FieldKind::Skip => {
                quote! {
                    #ident: Default::default(),
                }
            }
        };
        constructors.push(code);
    }

    quote! {
        #( #constructors )*
    }
}
