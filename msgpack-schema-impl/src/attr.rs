use quote::ToTokens;
use syn::{
    parse::{ParseStream, Parser},
    Attribute, Error, LitInt, LitStr, Result, Token,
};

pub struct Attrs<'a> {
    pub tag: Option<Tag<'a>>,
    pub optional: Option<Optional<'a>>,
    pub untagged: Option<Untagged<'a>>,
    pub flatten: Option<Flatten<'a>>,
    pub skip: Option<Skip<'a>>,
    pub default: Option<Default<'a>>,
}
#[derive(Clone)]
#[allow(dead_code)]
pub struct Skip<'a> {
    pub original: &'a Attribute,
}
#[derive(Clone)]
#[allow(dead_code)]
pub struct Default<'a> {
    pub original: &'a Attribute,
    pub path: String,
}
#[derive(Clone)]
pub struct Tag<'a> {
    pub original: &'a Attribute,
    pub tag: u32,
}

#[derive(Clone)]
pub struct Optional<'a> {
    pub original: &'a Attribute,
}

#[derive(Clone)]
pub struct Untagged<'a> {
    pub original: &'a Attribute,
}

#[derive(Clone)]
pub struct Flatten<'a> {
    pub original: &'a Attribute,
}

pub fn get(attrs: &[Attribute]) -> Result<Attrs> {
    let mut output = Attrs {
        tag: None,
        optional: None,
        untagged: None,
        flatten: None,
        skip: None,
        default: None,
    };

    for attr in attrs {
        if attr.path().is_ident("schema") {
            parse_schema_attribute(&mut output, attr)?;
        } else if attr.path().is_ident("tag") {
            let name_value = attr.meta.require_name_value()?;
            let parser = |input: ParseStream| {
                let lit_int = input.parse::<LitInt>()?;
                let tag = lit_int.base10_parse::<u32>()?;
                Ok(tag)
            };
            let tag = parser.parse2(name_value.value.to_token_stream())?;
            if output.tag.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[tag] attribute"));
            }
            output.tag = Some(Tag {
                original: attr,
                tag,
            })
        } else if attr.path().is_ident("untagged") {
            attr.meta.require_path_only()?;
            if output.untagged.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[untagged] attribute"));
            }
            output.untagged = Some(Untagged { original: attr });
        } else if attr.path().is_ident("optional") {
            attr.meta.require_path_only()?;
            if output.optional.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[optional] attribute"));
            }
            output.optional = Some(Optional { original: attr });
        } else if attr.path().is_ident("flatten") {
            attr.meta.require_path_only()?;
            if output.flatten.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[flatten] attribute"));
            }
            output.flatten = Some(Flatten { original: attr });
        } else if attr.path().is_ident("skip") {
            attr.meta.require_path_only()?;
            if output.skip.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[skip] attribute"));
            }
            output.skip = Some(Skip { original: attr });
        } else if attr.path().is_ident("default") {
            let name_value = attr.meta.require_name_value()?;
            let parser = |input: ParseStream| {
                let lit_str = input.parse::<LitStr>()?;
                Ok(lit_str.value())
            };
            let path = parser.parse2(name_value.value.to_token_stream())?;
            if output.default.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[default] attribute"));
            }
            output.default = Some(Default {
                original: attr,
                path,
            });
        }
    }
    Ok(output)
}

fn parse_schema_attribute<'a>(output: &mut Attrs<'a>, attr: &'a Attribute) -> Result<()> {
    syn::custom_keyword!(optional);
    syn::custom_keyword!(tag);
    syn::custom_keyword!(untagged);
    syn::custom_keyword!(flatten);
    syn::custom_keyword!(skip);
    syn::custom_keyword!(default);

    attr.parse_args_with(|input: ParseStream| {
        if let Some(_kw) = input.parse::<Option<optional>>()? {
            if output.optional.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[optional] attribute"));
            }
            output.optional = Some(Optional { original: attr });
            return Ok(());
        } else if let Some(_kw) = input.parse::<Option<untagged>>()? {
            if output.untagged.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[untagged] attribute"));
            }
            output.untagged = Some(Untagged { original: attr });
            return Ok(());
        } else if let Some(_kw) = input.parse::<Option<flatten>>()? {
            if output.flatten.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[flatten] attribute"));
            }
            output.flatten = Some(Flatten { original: attr });
            return Ok(());
        } else if let Some(_kw) = input.parse::<Option<skip>>()? {
            if output.skip.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[skip] attribute"));
            }
            output.skip = Some(Skip { original: attr });
            return Ok(());
        } else if let Some(_kw) = input.parse::<Option<tag>>()? {
            let _eq_token: Token![=] = input.parse()?;
            let lit_int = input.parse::<LitInt>()?;
            let tag = lit_int.base10_parse::<u32>()?;
            if output.tag.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[tag] attribute"));
            }
            output.tag = Some(Tag {
                original: attr,
                tag,
            });
            return Ok(());
        } else if let Some(_kw) = input.parse::<Option<default>>()? {
            let _eq_token: Token![=] = input.parse()?;
            let lit_str = input.parse::<LitStr>()?;
            let path = lit_str.value();
            if output.default.is_some() {
                return Err(Error::new_spanned(attr, "duplicate #[default] attribute"));
            }
            output.default = Some(Default {
                original: attr,
                path,
            });
            return Ok(());
        }
        let lit_int: LitInt = input.parse()?;
        let tag = lit_int.base10_parse::<u32>()?;
        if output.tag.is_some() {
            return Err(Error::new_spanned(attr, "duplicate #[tag] attribute"));
        }
        output.tag = Some(Tag {
            original: attr,
            tag,
        });
        Ok(())
    })
}

impl Attrs<'_> {
    pub fn disallow_tag(&self) -> Result<()> {
        if let Some(tag) = self.tag.clone() {
            return Err(Error::new_spanned(
                tag.original,
                "#[tag] at an invalid position",
            ));
        }
        Ok(())
    }

    pub fn disallow_optional(&self) -> Result<()> {
        if let Some(optional) = &self.optional {
            return Err(Error::new_spanned(
                optional.original,
                "#[optional] at an invalid position",
            ));
        }
        Ok(())
    }

    pub fn disallow_untagged(&self) -> Result<()> {
        if let Some(untagged) = &self.untagged {
            return Err(Error::new_spanned(
                untagged.original,
                "#[untagged] at an invalid position",
            ));
        }
        Ok(())
    }

    pub fn disallow_flatten(&self) -> Result<()> {
        if let Some(flatten) = &self.flatten {
            return Err(Error::new_spanned(
                flatten.original,
                "#[flatten] at an invalid position",
            ));
        }
        Ok(())
    }

    pub fn require_tag(&self, tokens: impl ToTokens) -> Result<()> {
        if self.tag.is_none() {
            return Err(Error::new_spanned(tokens, "no #[tag] given"));
        }
        Ok(())
    }
}

pub fn check_tag_uniqueness(tag: &Tag, tags: &mut Vec<u32>) -> Result<()> {
    if tags.iter().any(|t| *t == tag.tag) {
        return Err(Error::new_spanned(
            tag.original,
            "tag values must not be duplicate",
        ));
    }
    tags.push(tag.tag);
    Ok(())
}
