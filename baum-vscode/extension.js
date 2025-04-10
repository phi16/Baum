"use strict";
const vscode = require("vscode");
const lc = require("vscode-languageclient");

let client;

function log(s) {
  vscode.window.showInformationMessage(s);
}

function logError(s) {
  vscode.window.showErrorMessage(s);
}

async function start() {
  if (client) {
    logError("baum-vscode already started");
    return;
  }
  const serverOptions = {
    command: "baum-ls"
  };
  const clientOptions = {
    documentSelector: [{ scheme: "file", language: "baum" }]
  };
  try {
    client = new lc.LanguageClient("baum", "baum-ls-client", serverOptions, clientOptions);
    await client.start();
    log("baum-vscode started");
  } catch(e) {
    logError(`Failed to start baum-ls: ${e}`);
  }
}

async function restart() {
  if(!client) {
    logError("baum-vscode not started yet");
    return;
  }
  try {
    await client.restart();
    log("baum-vscode restarted");
  } catch(e) {
    logError(`Failed to restart baum-ls: ${e}`);
  }
}

async function stop() {
  if(!client) {
    logError("baum-vscode already stopped");
    return;
  }
  try {
    await client.stop();
    log("baum-vscode stopped");
    client = undefined;
  } catch(e) {
    logError(`Failed to stop baum-ls: ${e}`);
  }
}

async function activate(context) {
  log("baum-vscode activated");
  context.subscriptions.push(
    vscode.commands.registerCommand("baum-vscode.restartServer", () => restart())
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("baum-vscode.startServer", () => start())
  );
  context.subscriptions.push(
    vscode.commands.registerCommand("baum-vscode.stopServer", () => stop())
  );
  await start();
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