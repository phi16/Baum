use baum::TokenType;
use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::*;
use std::{error::Error, fs::File, io::Read};
mod baum;

fn token_type_list() -> Vec<SemanticTokenType> {
  vec![
    SemanticTokenType::PARAMETER, // 0
    SemanticTokenType::NAMESPACE, // 1
    SemanticTokenType::VARIABLE,  // 2
    SemanticTokenType::PROPERTY,  // 3
    SemanticTokenType::MACRO,     // 4
    SemanticTokenType::KEYWORD,   // 5
    SemanticTokenType::COMMENT,   // 6
    SemanticTokenType::STRING,    // 7
    SemanticTokenType::NUMBER,    // 8
    SemanticTokenType::OPERATOR,  // 9
  ]
}

fn from_token_type(t: TokenType) -> (u32, u32) {
  match t {
    TokenType::Def => (2, 1),
    TokenType::Bind => (0, 0),
    TokenType::Unknown => (2, 0),
    TokenType::Prop => (3, 0),
    TokenType::Module => (1, 0),

    TokenType::Syntax => (4, 0),
    TokenType::Keyword => (5, 0),
    TokenType::Symbol => (9, 0),
    TokenType::Precedence => (8, 0),

    TokenType::String => (7, 0),
    TokenType::Number => (8, 0),

    TokenType::Comment => (6, 0),
  }
}

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
  eprintln!("baum-ls started");
  let (connection, io_handles) = Connection::stdio();

  let server_capabilities = serde_json::to_value(&ServerCapabilities {
    position_encoding: Some(PositionEncodingKind::UTF16), // sadly
    semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
      SemanticTokensOptions {
        work_done_progress_options: Default::default(),
        legend: SemanticTokensLegend {
          token_types: token_type_list(),
          token_modifiers: vec![SemanticTokenModifier::DEFINITION],
        },
        range: Some(false),
        full: Some(SemanticTokensFullOptions::Bool(true)),
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

fn semantic_tokens(uri: &lsp_types::Uri) -> Result<SemanticTokensPartialResult, Box<dyn Error>> {
  let file_path = uri.as_str().replace("file:///c%3A/", "C:/");
  eprintln!("file_path = {:?}", uri.as_str());
  eprintln!("file_path = {:?}", file_path);
  let mut contents = String::new();
  File::open(file_path)?.read_to_string(&mut contents)?;
  let tokens = baum::tokenize_example(&contents).map_err(|_| "something wrong")?;
  let mut prev_line = 0;
  let mut prev_column = 0;
  let mut result = Vec::new();
  for t in tokens {
    if prev_line != t.line {
      prev_column = 0;
    }
    let (token_type, token_modifiers_bitset) = from_token_type(t.token_type);
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
  let result = SemanticTokensPartialResult { data: result };
  Ok(result)
}

fn main_loop(
  connection: Connection,
  _params: serde_json::Value,
) -> Result<(), Box<dyn Error + Send + Sync>> {
  for msg in &connection.receiver {
    match msg {
      Message::Request(req) => {
        if connection.handle_shutdown(&req)? {
          return Ok(());
        }
        eprintln!("main_loop req: {:?}", req);
        if let Ok((id, params)) = cast::<request::SemanticTokensFullRequest>(req) {
          eprintln!("semantic tokens request #{:?}: {:?}", id, params);
          let result = semantic_tokens(&params.text_document.uri).ok();
          let result = serde_json::to_value(&result).unwrap();
          let resp = Response {
            id,
            result: Some(result),
            error: None,
          };
          connection.sender.send(Message::Response(resp))?;
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
