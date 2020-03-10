// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as path from 'path';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export const activate = (context: vscode.ExtensionContext) => {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	// console.log('Congratulations, your extension "hello-world" is now active!');

	const simulate = vscode.commands.registerCommand('extension.simulate', () => {
		vscode.window.showInformationMessage('Simulating...');
		
		const terminalName = "Verishot: Simulate";
		const file = vscode.window.activeTextEditor?.document.fileName;

		const terminal = vscode.window.createTerminal(terminalName);
		terminal.show();
		terminal.sendText("HelloWorld file");
		
	});
	context.subscriptions.push(simulate);

	const visualise = vscode.commands.registerCommand('extension.visualise', () => {
		vscode.window.showInformationMessage('Visualising...');

		const terminalName = "Verishot: Visualise";
		const file = vscode.window.activeTextEditor?.document.fileName;

		const terminal = vscode.window.createTerminal(terminalName);

		terminal.show();
		terminal.sendText("HelloWorld file");
	});
	context.subscriptions.push(visualise);
};

// this method is called when your extension is deactivated
export function deactivate() {}
