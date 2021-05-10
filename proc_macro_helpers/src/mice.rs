use ::derive_syn_parse::Parse;
use ::syn::{Token, Ident, Attribute, token::{Brace, Paren}, punctuated::Punctuated, Type};

#[derive(Parse, Debug)]
struct UnaryPower {
    colon_token: Token![:],
    power: proc_macro2::Literal,
    trailing_comma: Option<Token![,]>,
}

#[derive(Parse, Debug)]
struct BinaryPower {
    colon_token: Token![:],
    #[paren]
    power_paren: Paren,
    #[inside(power_paren)]
    left_power: proc_macro2::Literal,
    #[inside(power_paren)]
    power_comma: Token![,],
    #[inside(power_paren)]
    right_power: proc_macro2::Literal,
}

#[derive(Parse, Debug)]
struct BindingPowers {
    unary_ident: Option<Ident>,
    #[parse_if(unary_ident.is_some() && unary_ident.clone().unwrap() == "unary")]
    unary: Option<UnaryPower>,
    binary_ident: Option<Ident>,
    #[parse_if(
        (binary_ident.is_some() && binary_ident.clone().unwrap() == "binary") ||
            (unary_ident.is_some() && unary_ident.clone().unwrap() == "binary")
    )]
    binary: Option<BinaryPower>,
}

#[derive(Parse, Debug)]
struct Variant {
    #[call(Attribute::parse_outer)]
    attrs: Vec<Attribute>,
    ident: Ident,
    #[brace]
    brace_token: Brace,
    #[inside(brace_token)]
    powers: BindingPowers,
}

#[derive(Parse, Debug)]
struct BindingPowerFnDecl {
    kind_token: Ident,
    arrow_token: Token![=>],
    #[call(Attribute::parse_outer)]
    fn_attrs: Vec<Attribute>,
    fn_token: Token![fn],
    fn_ident: Ident,
    #[paren]
    arg_paren: Paren,
    #[inside(arg_paren)]
    arg: Ident,
    ret_arrow_token: Token![->],
    ret_ty: Type,
    semicolon_token: Token![;],
}

#[derive(Parse, Debug)]
struct DeclOps {
    #[call(Attribute::parse_outer)]
    all_attrs: Vec<Attribute>,
    enum_token: Token![enum],
    #[call(Attribute::parse_outer)]
    sum_attrs: Vec<Attribute>,
    sum_ident: Ident,
    sum_equal_token: Token![=],
    #[call(Attribute::parse_outer)]
    unary_attrs: Vec<Attribute>,
    unary_ident: Ident,
    #[call(Attribute::parse_outer)]
    binary_attrs: Vec<Attribute>,
    binary_ident: Ident,

    #[brace]
    brace_token: Brace,
    #[inside(brace_token)]
    #[call(Punctuated::parse_terminated)]
    variants: Punctuated<Variant, Token![,]>,

    // TODO: make these optional
    unary_fn_decl: BindingPowerFnDecl,
    binary_fn_decl: BindingPowerFnDecl,
}

/// Like the [`dbg!`] macro, but uses [`Display`] instead of [`Debug`] for formatting.
#[allow(unused_macros)]
macro_rules! dsp {
    ($val:expr) => {{
        let val = $val;
        println!("[{}:{}] {} = {}",
                 file!(), line!(),
                 stringify!($val),
                 val);
        val
    }}
}


/// Declare a collection of unary prefix and binary infix operators,
/// with associated binding powers.
/// Generates fallible and infallible (via [`TryFrom`](::core::convert::TryFrom) and [`From`]) conversions
/// between the three sets of variants. (All; just unary; just binary.)
/// (To go from unary to binary or vice versa, go through the infallible conversion up to All,
/// then use the fallible conversion back down.)
pub fn decl_ops(input: ::proc_macro::TokenStream) -> ::proc_macro::TokenStream {
    let parse_input = input.clone();
    let parsed = syn::parse_macro_input!(parse_input as DeclOps);
    let DeclOps { all_attrs, enum_token, sum_attrs, sum_ident, unary_attrs,
                  unary_ident, binary_attrs, binary_ident, variants,
                  unary_fn_decl: BindingPowerFnDecl {
                      fn_attrs: unary_fn_attrs,
                      fn_token: unary_fn_token,
                      fn_ident: unary_fn_ident,
                      arg: unary_fn_arg,
                      ret_arrow_token: unary_fn_ret_arrow,
                      ret_ty: unary_ret_ty,
                      ..
                  },
                  binary_fn_decl: BindingPowerFnDecl {
                      fn_attrs: binary_fn_attrs,
                      fn_token: binary_fn_token,
                      fn_ident: binary_fn_ident,
                      arg: binary_fn_arg,
                      ret_arrow_token: binary_fn_ret_arrow,
                      ret_ty: binary_ret_ty,
                      ..
                  },
                  .. } = parsed;
    let to_variant = |&Variant { ref attrs, ref ident, .. }| {
        ::quote::quote! {
            #(#attrs)* #ident
        }
    };
    let sum_variant_idents = variants.iter().map(to_variant);
    let unary_variants = variants.iter().filter(|variant| variant.powers.unary.is_some());
    let unary_variant_idents = unary_variants.clone().map(to_variant);
    let unary_variant_power_arms = unary_variants.map(|variant| {
        let name = &variant.ident;
        let power = &variant.powers.unary.as_ref().unwrap().power;
        ::quote::quote! {
            #unary_ident :: #name => #power
        }
    });
    let try_unary_from_sum_arms = variants.iter().map(|variant| {
        let name = &variant.ident;
        if variant.powers.unary.is_some() {
            ::quote::quote! {
                #sum_ident :: #name => ::core::result::Result::Ok( #unary_ident :: #name )
            }
        } else {
            ::quote::quote! {
                #sum_ident :: #name => ::core::result::Result::Err(())
            }
        }
    });
    let sum_from_unary_arms = variants.iter().filter_map(|variant| {
        let name = &variant.ident;
        if variant.powers.unary.is_some() {
            Some(::quote::quote! {
                #unary_ident :: #name => #sum_ident :: #name
            })
        } else {
            None
        }
    });
    let binary_variants = variants.iter().filter(|variant| variant.powers.binary.is_some());
    let binary_variant_idents = binary_variants.clone().map(to_variant);
    let binary_variant_power_arms = binary_variants.map(|variant| {
        let name = &variant.ident;
        let BinaryPower {
            colon_token, power_paren, left_power, power_comma, right_power,
        } = &variant.powers.binary.as_ref().unwrap();
        let powers = ::quote::quote_spanned! {power_paren.span=> (#left_power #power_comma #right_power) };
        let arrow = ::quote::quote_spanned! {colon_token.span=> =>};
        ::quote::quote! {
            #binary_ident :: #name #arrow #powers
        }
    });
    let try_binary_from_sum_arms = variants.iter().map(|variant| {
        let name = &variant.ident;
        if variant.powers.binary.is_some() {
            ::quote::quote! {
                #sum_ident :: #name => ::core::result::Result::Ok( #binary_ident :: #name )
            }
        } else {
            ::quote::quote! {
                #sum_ident :: #name => ::core::result::Result::Err(())
            }
        }
    });
    let sum_from_binary_arms = variants.iter().filter_map(|variant| {
        let name = &variant.ident;
        if variant.powers.binary.is_some() {
            Some(::quote::quote! {
                #binary_ident :: #name => #sum_ident :: #name
            })
        } else {
            None
        }
    });
    // TODO: assign brace and paren spans correctly
    (::quote::quote! {
        #(#all_attrs)*
        #(#sum_attrs)*
        #enum_token #sum_ident {
            #(#sum_variant_idents),*
        }
        #(#all_attrs)*
        #(#unary_attrs)*
        #enum_token #unary_ident {
            #(#unary_variant_idents),*
        }
        #(#all_attrs)*
        #(#binary_attrs)*
        #enum_token #binary_ident {
            #(#binary_variant_idents),*
        }
        #(#unary_fn_attrs)*
        #unary_fn_token #unary_fn_ident ( #unary_fn_arg : #unary_ident ) #unary_fn_ret_arrow #unary_ret_ty {
            match #unary_fn_arg {
                #(#unary_variant_power_arms),*
            }
        }
        #(#binary_fn_attrs)*
        #binary_fn_token #binary_fn_ident ( #binary_fn_arg : #binary_ident )
        #binary_fn_ret_arrow #binary_ret_ty {
            match #binary_fn_arg {
                #(#binary_variant_power_arms),*
            }
        }
        impl ::core::convert::TryFrom<#sum_ident> for #unary_ident {
            type Error = ();
            fn try_from(op: #sum_ident) -> ::core::result::Result<Self, Self::Error> {
                match op {
                    #(#try_unary_from_sum_arms),*
                }
            }
        }

        impl ::core::convert::From<#unary_ident> for #sum_ident {
            fn from(op: #unary_ident) -> Self {
                match op {
                    #(#sum_from_unary_arms),*
                }
            }
        }

        impl ::core::convert::TryFrom<#sum_ident> for #binary_ident {
            type Error = ();
            fn try_from(op: #sum_ident) -> ::core::result::Result<Self, Self::Error> {
                match op {
                    #(#try_binary_from_sum_arms),*
                }
            }
        }

        impl ::core::convert::From<#binary_ident> for #sum_ident {
            fn from(op: #binary_ident) -> Self {
                match op {
                    #(#sum_from_binary_arms),*
                }
            }
        }
    }).into()
}
