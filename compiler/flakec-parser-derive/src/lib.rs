use proc_macro::TokenStream;
use quote::{quote, ToTokens as _};
use syn;

#[proc_macro_derive(Parse)]
pub fn parse_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_parse_macro(&ast)
}

fn impl_parse_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;

    let values = match &ast.data{
        syn::Data::Struct(s) => s.fields.iter().map(|f|(<Option<syn::Ident> as Clone>::clone(&f.ident).unwrap(), f.ty.clone())),
        _ => todo!()
    }.collect::<Vec::<_>>();

    let mut initializers: Vec<String> = vec![];

    for (name, _) in values{
        initializers.push(format!("{}: input.parse()?", name))
    }

    let binding = initializers.join(",\n");
    let code = syn::parse_str::<syn::FieldValue>(binding.as_str()).unwrap();

    let gen = quote! {
        impl Parse for #name {
            type Output = Self;

            fn parse(input: &mut lexer::stream::TokenStream) -> Option<Self> {
                Some(Self { #code })
            }
        }
    };

    gen.into()
}