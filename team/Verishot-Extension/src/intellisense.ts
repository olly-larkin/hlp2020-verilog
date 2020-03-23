import * as vscode from 'vscode';
import { writeFile } from 'fs';
import { spawn } from 'child_process';
import * as path from 'path';
import { showErrorMessageSeparated, binName, ensureDirectoryExists } from './utility';

// INTELLISENSE
let timeout: NodeJS.Timer | undefined = undefined;

const triggerIntellisense = (doc: vscode.TextDocument, diagCol: vscode.DiagnosticCollection, context: vscode.ExtensionContext) => {
	if (timeout) {
		clearTimeout(timeout);
		timeout = undefined;
	}
	timeout = setTimeout(() => execIntellisense(doc, diagCol, context), 1000);
};

const execIntellisense = (doc: vscode.TextDocument, diagCol: vscode.DiagnosticCollection, context: vscode.ExtensionContext) => {
	diagCol.clear();
	const storagePath = context.storagePath ? context.storagePath : "tmp";
	const tmpFilePath = path.join(storagePath, 'intellisenseTmp.txt');

	/// ensure this dir exists
	ensureDirectoryExists(tmpFilePath);

	writeFile(tmpFilePath, doc.getText(), (err) => {
		if (err) {
			showErrorMessageSeparated(err.message);
		}
		const args = [`--intellisense`, tmpFilePath];

		const s = spawn(binName, args);

		s.stdout.on('data', (data) => {
			const errorTexts: string[][] =
				data
					.toString()
					.split(`\n`)
					.map((line: string) => line.split(" ----- "));

			errorTexts.forEach((err: string[]) => {
				if (err.length === 3) {
					const line: number = parseInt(err[0]) - 1; // 1-indexed
					const char: number = parseInt(err[1]);
					const errMsg: string = err[2];
					const diagnostics: vscode.Diagnostic[] =
						[{
							severity: vscode.DiagnosticSeverity.Error,
							range: new vscode.Range(line, 0, line, char),
							message: errMsg,
						}];
					diagCol.set(doc.uri, diagnostics);
				}
			});
		});

		s.stderr.on('data', (data) => {
			console.error(`Intellisense Error: ${data.toString().trim()}`);
		});
	});
};

export const subscribeIntellisense = (context: vscode.ExtensionContext, diagCol: vscode.DiagnosticCollection) => {
	const vlanguageId = `verilog`;
	if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === vlanguageId) {
		triggerIntellisense(vscode.window.activeTextEditor.document, diagCol, context);
	}

	context.subscriptions.push(
		vscode.window.onDidChangeActiveTextEditor(editor => {
			if (editor && editor.document.languageId === vlanguageId) {
				triggerIntellisense(editor.document, diagCol, context);
			}
		})
	);

	context.subscriptions.push(
		vscode.workspace.onDidChangeTextDocument(e => {
			if (e.document.languageId === vlanguageId) { triggerIntellisense(e.document, diagCol, context); }
		}));

	context.subscriptions.push(
		vscode.workspace.onDidCloseTextDocument(doc => diagCol.delete(doc.uri))
	);
};
