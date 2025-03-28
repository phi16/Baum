use lang::TokenType;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::*;
use std::{error::Error, fs::File, io::Read};
mod lang;
mod server;

struct TokenTypeSignature(pub u32);

impl TokenTypeSignature {
  pub const PARAMETER: Self = Self(0);
  pub const NAMESPACE: Self = Self(1);
  pub const VARIABLE: Self = Self(2);
  pub const PROPERTY: Self = Self(3);
  pub const MACRO: Self = Self(4);
  pub const KEYWORD: Self = Self(5);
  pub const COMMENT: Self = Self(6);
  pub const STRING: Self = Self(7);
  pub const NUMBER: Self = Self(8);
  pub const OPERATOR: Self = Self(9);
}

fn token_type_list() -> Vec<SemanticTokenType> {
  vec![
    SemanticTokenType::PARAMETER,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::MACRO,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::OPERATOR,
  ]
}

fn token_modifiers_list() -> Vec<SemanticTokenModifier> {
  vec![
    SemanticTokenModifier::DEFINITION,
    SemanticTokenModifier::DEFAULT_LIBRARY,
  ]
}

fn from_token_type(t: TokenType) -> Option<(u32, u32)> {
  type Sig = TokenTypeSignature;
  let res = match t {
    TokenType::Def => (Sig::VARIABLE, 1),
    TokenType::Ext => (Sig::VARIABLE, 0),
    TokenType::Bind => (Sig::PARAMETER, 0),
    TokenType::Unknown => return None,
    TokenType::Prop => (Sig::PROPERTY, 0),
    TokenType::Module => (Sig::NAMESPACE, 0),

    TokenType::SynDefIdent => (Sig::PARAMETER, 0),
    TokenType::SynDefExpr => (Sig::VARIABLE, 0),
    TokenType::BuiltinSyntax => (Sig::MACRO, 2),
    TokenType::UserSyntax => (Sig::MACRO, 0),
    TokenType::Keyword => (Sig::KEYWORD, 0),
    TokenType::Symbol => (Sig::OPERATOR, 0),
    TokenType::Precedence => (Sig::NUMBER, 0),

    TokenType::String => (Sig::STRING, 0),
    TokenType::Number => (Sig::NUMBER, 0),

    TokenType::Comment => (Sig::COMMENT, 0),
  };
  Some((res.0 .0, res.1))
}

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
  eprintln!("baum-ls started");
  let (connection, io_handles) = Connection::stdio();

  let server_capabilities = serde_json::to_value(&ServerCapabilities {
    diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
      identifier: None,
      inter_file_dependencies: false,
      workspace_diagnostics: false,
      work_done_progress_options: Default::default(),
    })),
    position_encoding: Some(PositionEncodingKind::UTF16), // sadly
    semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
      SemanticTokensOptions {
        legend: SemanticTokensLegend {
          token_types: token_type_list(),
          token_modifiers: token_modifiers_list(),
        },
        range: Some(true),
        full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
        work_done_progress_options: Default::default(),
      },
    )),
    ..Default::default()
  })
  .unwrap();
  let params = connection.initialize(server_capabilities)?;
  main_loop(connection, params)?;
  io_handles.join()?;
  eprintln!("baum-ls terminated");
  Ok(())
}

fn semantic_tokens(
  uri: &lsp_types::Uri,
) -> Result<(Vec<SemanticToken>, Vec<lang::Diagnostic>), Box<dyn Error>> {
  let file_path = uri.as_str().replace("file:///c%3A/", "C:/");
  eprintln!("file_path = {:?}", uri.as_str());
  eprintln!("file_path = {:?}", file_path);
  let mut contents = String::new();
  File::open(file_path)?.read_to_string(&mut contents)?;
  let (tokens, diagnostics) = lang::tokenize_example(&contents);
  let mut prev_line = 0;
  let mut prev_column = 0;
  let mut result = Vec::new();
  for t in tokens {
    if prev_line != t.line {
      prev_column = 0;
    }
    if let Some((token_type, token_modifiers_bitset)) = from_token_type(t.token_type) {
      result.push(SemanticToken {
        delta_line: t.line - prev_line,
        delta_start: t.column - prev_column,
        length: t.length,
        token_type,
        token_modifiers_bitset,
      });
      prev_line = t.line;
      prev_column = t.column;
    }
  }
  Ok((result, diagnostics))
}

fn main_loop(
  connection: Connection,
  _params: serde_json::Value,
) -> Result<(), Box<dyn Error + Send + Sync>> {
  eprintln!("params = {:?}", _params);
  let mut st_id = 0;
  for msg in &connection.receiver {
    match msg {
      Message::Request(req) => {
        if connection.handle_shutdown(&req)? {
          return Ok(());
        }
        eprintln!("main_loop req: {:?}", req);
        if let Some((id, text_document)) = {
          if let Ok((id, params)) = cast::<request::SemanticTokensFullRequest>(req.clone()) {
            eprintln!("semantic tokens request #{:?}: {:?}", id, params);
            Some((id, params.text_document))
          } else if let Ok((id, params)) =
            cast::<request::SemanticTokensFullDeltaRequest>(req.clone())
          {
            eprintln!("semantic tokens delta request #{:?}: {:?}", id, params);
            Some((id, params.text_document))
          } else if let Ok((id, params)) = cast::<request::SemanticTokensRangeRequest>(req) {
            eprintln!("semantic tokens range request #{:?}: {:?}", id, params);
            Some((id, params.text_document))
          } else {
            None
          }
        } {
          let (result, diags) = match semantic_tokens(&text_document.uri) {
            Ok((r, diags)) => (r, diags),
            Err(_) => (vec![], vec![]),
          };
          let result = SemanticTokensResult::Tokens(SemanticTokens {
            result_id: Some(st_id.to_string()),
            data: result,
          });
          let result: <request::SemanticTokensFullRequest as lsp_types::request::Request>::Result =
            Some(result);
          st_id += 1;
          let result = serde_json::to_value(&result).unwrap();
          let resp = Response {
            id,
            result: Some(result),
            error: None,
          };
          connection.sender.send(Message::Response(resp))?;
          let mut diagnostics = Vec::new();
          for d in diags {
            diagnostics.push(Diagnostic {
              range: Range {
                start: Position {
                  line: d.begin_line,
                  character: d.begin_column,
                },
                end: Position {
                  line: d.end_line,
                  character: d.end_column,
                },
              },
              severity: Some(DiagnosticSeverity::ERROR),
              message: d.message,
              ..Default::default()
            });
          }
          let params = PublishDiagnosticsParams {
            uri: text_document.uri,
            diagnostics,
            version: None,
          };
          connection.sender.send(Message::Notification(Notification {
            method: "textDocument/publishDiagnostics".to_string(),
            params: serde_json::to_value(&params).unwrap(),
          }))?;
          continue;
        }
      }
      Message::Response(resp) => {
        eprintln!("main_loop resp: {:?}", resp);
      }
      Message::Notification(not) => {
        eprintln!("main_loop not: {:?}", not);
      }
    }
  }
  Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
  R: lsp_types::request::Request,
  R::Params: serde::de::DeserializeOwned,
{
  req.extract(R::METHOD)
}
