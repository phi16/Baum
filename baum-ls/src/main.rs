use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{
  request, PositionEncodingKind, SemanticToken, SemanticTokenModifier, SemanticTokenType,
  SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
  SemanticTokensPartialResult, SemanticTokensServerCapabilities, ServerCapabilities,
};
use std::{error::Error, fs::File, io::Read};
mod baum;

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
  eprintln!("baum-ls started");
  let (connection, io_handles) = Connection::stdio();

  let server_capabilities = serde_json::to_value(&ServerCapabilities {
    position_encoding: Some(PositionEncodingKind::UTF16), // sadly
    semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
      SemanticTokensOptions {
        work_done_progress_options: Default::default(),
        legend: SemanticTokensLegend {
          token_types: vec![
            SemanticTokenType::PARAMETER,
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
            SemanticTokenType::OPERATOR,
          ],
          token_modifiers: vec![SemanticTokenModifier::DEFINITION],
        },
        range: Some(false),
        full: Some(SemanticTokensFullOptions::Bool(true)),
      },
    )),
    ..Default::default()
  })
  .unwrap();
  eprintln!("server_capabilities = {:?}", server_capabilities);
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
    result.push(SemanticToken {
      delta_line: t.line - prev_line,
      delta_start: t.column - prev_column,
      length: t.length,
      token_type: t.token_type,
      token_modifiers_bitset: 0,
    });
    prev_line = t.line;
    prev_column = t.column;
  }
  let result = SemanticTokensPartialResult { data: result };
  Ok(result)
}

fn main_loop(
  connection: Connection,
  params: serde_json::Value,
) -> Result<(), Box<dyn Error + Send + Sync>> {
  eprintln!("main_loop: params = {:?}", params);
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
