"use strict";
const vscode = require("vscode");
const lc = require("vscode-languageclient");

let client;

async function restart() {
  vscode.window.showInformationMessage("baum-vscode restating...");
  try {
    if(client === undefined) {
      throw new Error();
    }
    await client.restart();
  } catch(e) {
    vscode.window.showErrorMessage(`Failed to restart baum-ls: ${e}`);
  }
}

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand("baum-vscode.restartServer", () => restart())
  );
  vscode.window.showInformationMessage("baum-vscode activated");
  try {
    const serverOptions = {
      command: "baum-ls"
    };
    const clientOptions = {
      documentSelector: [{ scheme: "file", language: "baum" }]
    };
    client = new lc.LanguageClient("baum", "baum-ls-client", serverOptions, clientOptions);
    context.subscriptions.push(client.start());
  } catch(e) {
    vscode.window.showErrorMessage(`Failed to start baum-ls: ${e}`);
  }
}

function deactivate() {
  if (client) {
    return client.stop();
  }
}

module.exports = {
  activate,
  deactivate
};