// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { execVerishot } from './verishot';
import { VerishotMode, verishotExists } from './utility';
import { newModuleHandler, newProjectHandler, deleteModuleHandler } from './project';
import { subscribeIntellisense } from './intellisense';

export const activate = (context: vscode.ExtensionContext) => {
	/// need to make sure `verishot` exists in PATH variable, otherwise throw an error and exit
	if (!verishotExists()) {
		vscode.window.showErrorMessage(`\`verishot\` is not found in your system. Please make sure it is installed and in your PATH variables.`);
		return;
	}

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
	const diagnosticCollection = vscode.languages.createDiagnosticCollection();
	subscribeIntellisense(context, diagnosticCollection);




	// PROJECT
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
