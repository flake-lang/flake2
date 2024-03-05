use lexgen::lexer;

use crate::token::TokenKind;

use std::string::String as RustString;

use TokenKind::*;
use crate::Keyword::*;
use crate::Literal::*;
use crate::token::BasicToken::*;

#[derive(Debug, Clone, PartialEq, Default, Eq)]
pub struct LexerState {
    is_comment: bool,
    string_buf: RustString,
    str_delim: Quote,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Quote {
    #[default]
    Single,
    Double,
}

lexer! {
    pub Lexer(LexerState) -> TokenKind<'input>;

    let whitespace = [' ' '\t' '\n'] | "\r\n";

    rule Init{
        $whitespace,
 

        // ==== Basic ====
        "+"   =  Basic(Plus),
        "-"   =  Basic(Minus),
        "*"   =  Basic(Star),
        "/"   =  Basic(Slash),
        "%"   =  Basic(Percent),
        "^"   =  Basic(Caret),
        "#"   =  Basic(Hash),
        "=="  = Basic(EqEq),
        "~="  = Basic(TildeEq),
        "<="  = Basic(LtEq),
        ">="  = Basic(GtEq),
        "!="  = Basic(NotEq),
        "<"   =  Basic(Lt),
        ">"   =  Basic(Gt),
        "="   =  Basic(Eq),
        "("   =  Basic(LParen),
        ")"   =  Basic(RParen),
        "{"   =  Basic(LBrace),
        "}"   =  Basic(RBrace),
        "]"   =  Basic(RBracket),
        "["   =  Basic(LBracket),
        ";"   =  Basic(Semicolon),
        ":"   =  Basic(Colon),
        "!"   =  Basic(ExplMark),
        ","   =  Basic(Comma),
        "."   =  Basic(Dot),
        ".."  = Basic(DotDot),
        "..." = Basic(DotDotDot),

        // ==== Keywords ====
        "fn" = Keyword(Fn),
        "let" = Keyword(Let),
        "construct" = Keyword(Construct),
        "for" = Keyword(For),
        "if" = Keyword(If),
        "else" = Keyword(Else),
        "while" = Keyword(While),
        "return" = Keyword(Return),
        "break" = Keyword(Break),
        "continue" = Keyword(Continue),
        
        // ==== Semi-Keywords ====
        "true" = Literal(Boolean(true)),
        "false" = Literal(Boolean(false)),

        // ==== Other ====
        '"' => |lexer| {
            lexer.state().str_delim = Quote::Double;
            lexer.state().string_buf.clear();
            lexer.switch(LexerRule::String)
        },


        '\'' => |lexer| {
            lexer.state().str_delim = Quote::Single;
            lexer.state().string_buf.clear();
            lexer.switch(LexerRule::String)
        },

        let ident_init = ['a'-'z' 'A'-'Z' '_'];
        let ident_subseq = $ident_init | ['0'-'9'];

        $ident_init $ident_subseq* => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(TokenKind::Identifier(match_))
        },

        let digit = ['0'-'9'];
        let hex_digit = ['a'-'f' 'A'-'F' '0'-'9'];

        $digit+ ('.'? $digit+ (('e' | 'E') '+'? $digit+)?)? => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(TokenKind::Literal(Number(match_)))
        },

        $digit+ ('.'? $digit+ (('e' | 'E') '-' $digit+)?)? => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(TokenKind::Literal(SignedNumber(match_)))
        },

        "0x" $hex_digit+ => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(TokenKind::Literal(Number(match_)))
        },

        "//" => |lexer| {
            lexer.state().is_comment = true;
            lexer.switch(LexerRule::Comment)
        },

    }

    rule Comment {
        '\n' => |lexer| lexer.switch(LexerRule::Init),

        _ => |lexer| lexer.continue_(),
    }

    rule String {
        '"' => |lexer| {
            if lexer.state().str_delim == Quote::Double {
                let str = lexer.state().string_buf.clone();
                lexer.switch_and_return(LexerRule::Init, TokenKind::Literal(OwnedString(str)))
            } else {
                lexer.state().string_buf.push('"');
                lexer.continue_()
            }
        },

        "'" => |lexer| {
            if lexer.state().str_delim == Quote::Single {
                let str = lexer.state().string_buf.clone();
                lexer.switch_and_return(LexerRule::Init, TokenKind::Literal(OwnedString(str)))
            } else {
                lexer.state().string_buf.push('\'');
                lexer.continue_()
            }
        },

        "\\a" => |lexer| {
            lexer.state().string_buf.push('\u{7}');
            lexer.continue_()
        },

        "\\b" => |lexer| {
            lexer.state().string_buf.push('\u{8}');
            lexer.continue_()
        },

        "\\f" => |lexer| {
            lexer.state().string_buf.push('\u{c}');
            lexer.continue_()
        },

        "\\n" => |lexer| {
            lexer.state().string_buf.push('\n');
            lexer.continue_()
        },

        "\\r" => |lexer| {
            lexer.state().string_buf.push('\r');
            lexer.continue_()
        },

        "\\t" => |lexer| {
            lexer.state().string_buf.push('\t');
            lexer.continue_()
        },

        "\\v" => |lexer| {
            lexer.state().string_buf.push('\u{b}');
            lexer.continue_()
        },

        "\\\\" => |lexer| {
            lexer.state().string_buf.push('\\');
            lexer.continue_()
        },

        "\\\"" => |lexer| {
            lexer.state().string_buf.push('"');
            lexer.continue_()
        },

        "\\'" => |lexer| {
            lexer.state().string_buf.push('\'');
            lexer.continue_()
        },

        "\\\n" => |lexer| {
            lexer.state().string_buf.push('\n');
            lexer.continue_()
        },

        _ => |lexer| {
            let char = lexer.match_().chars().next_back().unwrap();
            lexer.state().string_buf.push(char);
            lexer.continue_()
        },
    }

}