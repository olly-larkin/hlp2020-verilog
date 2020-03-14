import * as vscode from 'vscode';
import * as cp from 'child_process';

// INTELLISENSE
let timeout: NodeJS.Timer | undefined = undefined;

const triggerIntellisense = (doc: vscode.TextDocument, diagCol: vscode.DiagnosticCollection) => {
	if (timeout) {
		clearTimeout(timeout);
		timeout = undefined;
	}
	timeout = setTimeout(() => execIntellisense(doc, diagCol), 1000);
};

const execIntellisense = (doc: vscode.TextDocument, diagCol: vscode.DiagnosticCollection) => {
	diagCol.clear();
	const code = doc.getText().split("\n").map(line => "\"" + line + "\"").join(" ");
	cp.exec(`verishot --intellisense ${code}`, (err: any, stdout: any, stderr: any) => {
		if (stdout && stdout.length) {
			let splitted = stdout.split("#####");
			if (splitted.length === 3) {
				let line: number = parseInt(splitted[0]) - 1; // 1-indexed
				let char: number = parseInt(splitted[1]);
				let errMsg: string = splitted[2];
				const diagnostics: vscode.Diagnostic[] =
					[{
						severity: vscode.DiagnosticSeverity.Error,
						range: new vscode.Range(line, 0, line, char),
						message: errMsg,
					}];
				diagCol.set(doc.uri, diagnostics);
			}
		}
	});
};

export const subscribeIntellisense = (context: vscode.ExtensionContext, diagCol: vscode.DiagnosticCollection) => {
	if (vscode.window.activeTextEditor) {
		triggerIntellisense(vscode.window.activeTextEditor.document, diagCol);
	}

	context.subscriptions.push(
		vscode.window.onDidChangeActiveTextEditor(editor => {
			if (editor) {
				triggerIntellisense(editor.document, diagCol);
			}
		})
	);

	context.subscriptions.push(
		vscode.workspace.onDidChangeTextDocument(e => triggerIntellisense(e.document, diagCol))
	);

	context.subscriptions.push(
		vscode.workspace.onDidCloseTextDocument(doc => diagCol.delete(doc.uri))
	);
};
