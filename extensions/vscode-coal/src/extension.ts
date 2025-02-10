import * as vscode from "vscode";
import { exec } from "node:child_process";

export function activate(context: vscode.ExtensionContext) {
  const disposable = vscode.languages.registerDocumentFormattingEditProvider(
    "coal",
    {
      provideDocumentFormattingEdits(document) {
        return new Promise((resolve, reject) => {
          const input = document.getText();

          exec(`coal fmt '${input}'`, (err, stdout, stderr) => {
            if (err) {
              reject(`Formatting failed: ${stderr}`);
              return;
            }
            const edit = new vscode.TextEdit(
              document.validateRange(
                new vscode.Range(0, 0, document.lineCount, 0),
              ),
              stdout,
            );
            resolve([edit]);
          });
        });
      },
    },
  );

  context.subscriptions.push(disposable);
}
