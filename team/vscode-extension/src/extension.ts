// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { execIntellisense } from './intellisense';
import { execVerishot } from './verishot';
import { VerishotMode } from './util';


export const activate = (context: vscode.ExtensionContext) => {
	// console.log("[VERISHOT ACTIVATED]");
	const lint = vscode.commands.registerCommand('extension.lint', () => {
		vscode.window.showInformationMessage('Linting...');
		execVerishot(VerishotMode.lint);
	});
	context.subscriptions.push(lint);

	const simulate = vscode.commands.registerCommand('extension.simulate', () => {
		vscode.window.showInformationMessage('Simulating...');
		execVerishot(VerishotMode.simulate);
	});
	context.subscriptions.push(simulate);

	const visualise = vscode.commands.registerCommand('extension.visualise', () => {
		vscode.window.showInformationMessage('Visualising...');
		execVerishot(VerishotMode.visualise);
	});
	context.subscriptions.push(visualise);


	// INTELLISENSE
	let timeout: NodeJS.Timer | undefined = undefined;
	const diagnosticCollection = vscode.languages.createDiagnosticCollection();
	
	const triggerIntellisense = (editor: vscode.TextEditor, doc: vscode.TextDocument) => {
		if (timeout) {
			clearTimeout(timeout);
			timeout = undefined;
		}
		timeout = setTimeout(() => execIntellisense(editor, doc, diagnosticCollection), 1000);
	};

	vscode.workspace.onDidSaveTextDocument((doc: vscode.TextDocument) => {
		const editor: vscode.TextEditor | undefined = vscode.window.activeTextEditor;
		if (editor && editor.document === doc) {
			triggerIntellisense(editor, doc);
		}
	}, null, context.subscriptions);
};

// this method is called when your extension is deactivated
export function deactivate() {}
