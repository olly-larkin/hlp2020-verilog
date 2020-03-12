// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { execIntellisense } from './intellisense';
import { execVerishot } from './verishot';
import { VerishotMode } from './utility';
import { newModuleHandler, newProjectHandler, deleteModuleHandler } from './project';
import { getInput } from './input';


export const activate = (context: vscode.ExtensionContext) => {
	const playground = vscode.commands.registerCommand('extension.playground', () => {
		vscode.window.showInformationMessage('playground.');
		const result = vscode.window.showInputBox({
			value: 'abcdef',
			valueSelection: [2, 4],
			placeHolder: 'For example: fedcba. But not: 123',
			validateInput: text => {
				vscode.window.showInformationMessage(`Validating: ${text}`);
				return text === '123' ? 'Not 123!' : null;
			}
		});
		vscode.window.showInformationMessage(`Got: ${result}`);
	});
	context.subscriptions.push(playground);




	// --lint, --simulate, --visualise
	const lint = vscode.commands.registerCommand('extension.lint', () => {
		execVerishot(VerishotMode.lint);
	});
	context.subscriptions.push(lint);

	const simulate = vscode.commands.registerCommand('extension.simulate', () => {
		execVerishot(VerishotMode.simulate);
	});
	context.subscriptions.push(simulate);

	const visualise = vscode.commands.registerCommand('extension.visualise', () => {
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





	const newproject = vscode.commands.registerCommand('extension.newproject', () => {
		newProjectHandler();
	});
	context.subscriptions.push(newproject);

	const newmodule = vscode.commands.registerCommand('extension.newmodule', () => {
		newModuleHandler();
	});
	context.subscriptions.push(newmodule);

	const deletemodule = vscode.commands.registerCommand('extension.deletemodule', () => {
		deleteModuleHandler();
	});
	context.subscriptions.push(deletemodule);

};

// this method is called when your extension is deactivated
export function deactivate() { }
