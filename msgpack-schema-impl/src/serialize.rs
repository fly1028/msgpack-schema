use std::str::FromStr;

use crate::attr;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Error, Field, Fields, FieldsNamed, FieldsUnnamed,
    Result,
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
                        "empty tuple structs as serialize are not supported",
                    )),
                    1 => derive_newtype_struct(node, strut, &fields.unnamed[0]),
                    _ => derive_tuple_struct(node, strut, fields),
                }
            }
            Fields::Unit => {
                attrs.disallow_untagged()?;
                Err(Error::new_spanned(
                    node,
                    "unit structs as serialize are not supported",
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
            "union as serialize are not supported",
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

    let count_fields_body = generate_field_count(
        &fields,
        named_fields.named.len() as u32,
        |ident| quote! { self.#ident },
    );

    let serialize_fields_body =
        generate_field_serialization(&fields, |ident| quote! { self.#ident });

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Serialize for #ty #ty_generics #where_clause {
            fn serialize(&self, serializer: &mut ::msgpack_schema::Serializer) -> Result<(), ::msgpack_schema::SerializeError>{
                let count = <Self as ::msgpack_schema::StructSerialize>::count_fields(self);
                serializer.serialize_map(count)?;
                <Self as ::msgpack_schema::StructSerialize>::serialize_fields(self, serializer)?;
                Ok(())
            }
        }

        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::StructSerialize for #ty #ty_generics #where_clause {
            fn count_fields(&self) -> u32 {
                #count_fields_body
            }

            fn serialize_fields(&self, serializer: &mut ::msgpack_schema::Serializer) -> Result<(), ::msgpack_schema::SerializeError>{
                #serialize_fields_body
                Ok(())
            }
        }
    };

    Ok(gen)
}

fn derive_newtype_struct(
    node: &DeriveInput,
    _strut: &DataStruct,
    field: &Field,
) -> Result<TokenStream> {
    let ty = &node.ident;
    let (impl_generics, ty_generics, where_clause) = node.generics.split_for_impl();

    let attrs = attr::get(&field.attrs)?;
    attrs.disallow_tag()?;
    attrs.disallow_optional()?;
    attrs.disallow_untagged()?;
    attrs.disallow_flatten()?;

    let fn_body = quote! {
        serializer.serialize(&self.0)?;
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Serialize for #ty #ty_generics #where_clause {
            fn serialize(&self, serializer: &mut ::msgpack_schema::Serializer) -> Result<(), ::msgpack_schema::SerializeError>{
                #fn_body
                Ok(())
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
    let field_specs = (0..count).map(|n| TokenStream::from_str(&format!("{}", n)).unwrap());

    let fn_body = quote! {
        serializer.serialize_array(#count)?;
        #( serializer.serialize(&self.#field_specs)?; )*
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Serialize for #ty #ty_generics #where_clause {
            fn serialize(&self, serializer: &mut ::msgpack_schema::Serializer) -> Result<(), ::msgpack_schema::SerializeError>{
                #fn_body
                Ok(())
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
                    // Create a struct-like implementation for a named fields variant
                    // We'll serialize as an array of [tag, map]

                    // Process fields using common helper
                    let variant_fields = process_fields(fields)?;

                    // Generate pattern matching for field extraction and field names for struct pattern
                    let field_names = variant_fields
                        .iter()
                        .map(|(ident, _, _)| quote! { #ident })
                        .collect::<Vec<_>>();

                    // Generate field count calculation using common helper
                    let field_count_code = generate_field_count(
                        &variant_fields,
                        fields.named.len() as u32,
                        |ident| quote! { #ident },
                    );

                    // Generate field serialization using common helper
                    let serialize_fields_code =
                        generate_field_serialization(&variant_fields, |ident| quote! { #ident });

                    // Add the clause for this named-fields variant
                    clauses.push(quote! {
                        Self::#ident { #(#field_names),* } => {
                            serializer.serialize_array(2)?;
                            serializer.serialize(#tag)?;

                            // Serialize as a map, similar to struct serialization
                            let count = {
                                #field_count_code
                            };
                            serializer.serialize_map(count)?;

                            // Serialize the fields
                            #serialize_fields_code
                        }
                    });
                }
                Fields::Unnamed(fields) => {
                    let len = fields.unnamed.len() as u32;
                    match len {
                        0 => {
                            clauses.push(quote! {
                                Self::#ident() => {
                                    serializer.serialize(#tag)?;
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
                                Self::#ident(value) => {
                                    serializer.serialize_array(2)?;
                                    serializer.serialize(#tag)?;
                                    serializer.serialize(value)?;
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
                        Self::#ident => {
                            serializer.serialize(#tag)?;
                        }
                    });
                }
            }
        }

        quote! {
            match self {
                #( #clauses )*
            }
        }
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Serialize for #ty #ty_generics #where_clause {
            fn serialize(&self, serializer: &mut ::msgpack_schema::Serializer) -> Result<(), ::msgpack_schema::SerializeError>{
                #fn_body
                Ok(())
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
        for (variant, _field) in &members {
            let ident = variant.ident.clone();
            clauses.push(quote! {
                Self::#ident(value) => {
                    serializer.serialize(value)?
                }
            });
        }

        quote! {
            match self {
                #( #clauses )*
            }
        }
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Serialize for #ty #ty_generics #where_clause {
            fn serialize(&self, serializer: &mut ::msgpack_schema::Serializer) -> Result<(), ::msgpack_schema::SerializeError>{
                #fn_body
                Ok(())
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
            let ident = field.ident.clone().unwrap();
            let attrs = attr::get(&field.attrs)?;
            attrs.disallow_tag()?;
            attrs.disallow_optional()?;
            attrs.disallow_untagged()?;
            attrs.disallow_flatten()?;
            members.push(ident);
        }

        let len = members.len() as u32;

        let mut pushes = vec![];
        for ident in &members {
            let push = quote! {
                serializer.serialize(&self.#ident)?;
            };
            pushes.push(push);
        }

        quote! {
            serializer.serialize_array(#len)?;
            #( #pushes )*
        }
    };

    let gen = quote! {
        #[allow(unused_qualifications)]
        impl #impl_generics ::msgpack_schema::Serialize for #ty #ty_generics #where_clause {
            fn serialize(&self, serializer: &mut ::msgpack_schema::Serializer) -> Result<(), ::msgpack_schema::SerializeError>{
                #fn_body
                Ok(())
            }
        }
    };

    Ok(gen)
}

/// Processes fields to extract field kind information
fn process_fields(fields: &FieldsNamed) -> Result<Vec<(proc_macro2::Ident, syn::Type, FieldKind)>> {
    let mut processed_fields = vec![];
    let mut tags = vec![];

    for field in &fields.named {
        let ident = field.ident.clone().unwrap();
        let ty = field.ty.clone();
        let attrs = attr::get(&field.attrs)?;
        attrs.disallow_untagged()?;

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
            if attrs.optional.is_some() {
                FieldKind::Optional(tag)
            } else {
                FieldKind::Ordinary(tag)
            }
        };

        processed_fields.push((ident, ty, kind));
    }

    Ok(processed_fields)
}

/// Generates field count calculation code
fn generate_field_count(
    fields: &Vec<(proc_macro2::Ident, syn::Type, FieldKind)>,
    max_len: u32,
    self_accessor: impl Fn(&proc_macro2::Ident) -> TokenStream,
) -> TokenStream {
    let mut decs = vec![];

    for (ident, ty, kind) in fields {
        let field_access = self_accessor(ident);
        match kind {
            FieldKind::Flatten => {
                decs.push(quote! {
                    max_len -= 1;
                    max_len += <#ty as ::msgpack_schema::StructSerialize>::count_fields(&#field_access);
                });
            }
            FieldKind::Optional(_) => {
                decs.push(quote! {
                    if #field_access.is_none() {
                        max_len -= 1;
                    }
                });
            }
            FieldKind::Ordinary(_) => {}
            FieldKind::Skip => {
                decs.push(quote! {
                    max_len -= 1; // Reduce count for skipped field
                });
            }
        }
    }

    quote! {
        let mut max_len: u32 = #max_len;
        #( #decs )*
        max_len
    }
}

/// Generates field serialization code
fn generate_field_serialization(
    fields: &Vec<(proc_macro2::Ident, syn::Type, FieldKind)>,
    self_accessor: impl Fn(&proc_macro2::Ident) -> TokenStream,
) -> TokenStream {
    let mut pushes = vec![];

    for (ident, ty, kind) in fields {
        let field_access = self_accessor(ident);
        let code = match kind {
            FieldKind::Ordinary(tag) => {
                quote! {
                    serializer.serialize(#tag)?;
                    serializer.serialize(&#field_access)?;
                }
            }
            FieldKind::Optional(tag) => {
                quote! {
                    if let Some(value) = &#field_access {
                        serializer.serialize(#tag)?;
                        serializer.serialize(value)?;
                    }
                }
            }
            FieldKind::Flatten => {
                quote! {
                    <#ty as ::msgpack_schema::StructSerialize>::serialize_fields(&#field_access, serializer)?;
                }
            }
            FieldKind::Skip => {
                quote! {} // Skip this field, nothing to push
            }
        };
        pushes.push(code);
    }

    quote! {
        #( #pushes )*
    }
}
