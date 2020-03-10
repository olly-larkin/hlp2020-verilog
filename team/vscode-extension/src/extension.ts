// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

enum VerishotMode {
	simulate,
	visualise,
}

const checkFilePath = (filePath: string | undefined): boolean => {
	if (!filePath) {
		vscode.window.showErrorMessage("ERROR: No active file found. Please open a `.v` Verilog file");
		return false;
	}
	else if (filePath.substr(filePath.length - 2) !== ".v") {
		vscode.window.showErrorMessage("ERROR: Only `.v` Verilog files are allowed");
		return false;
	}
	return true;
};

const execVerishot = (verishotMode: number) => {
	const filePath = vscode.window.activeTextEditor?.document.fileName;
	const workspacePath = vscode.workspace.rootPath;

	if (!checkFilePath(filePath)) {
		return;
	}
	if (verishotMode === VerishotMode.simulate) {
		const terminalName = "Verishot: Simulate";
		const terminal = vscode.window.createTerminal(terminalName);
		terminal.show();
		terminal.sendText(`verishot ${filePath} --simulate ${workspacePath}`);
	}
	else if (verishotMode === VerishotMode.visualise) {
		const terminalName = "Verishot: Visualise";
		const terminal = vscode.window.createTerminal(terminalName);
		terminal.show();
		terminal.sendText(`verishot ${filePath} --visualise ${workspacePath}`);
	}
	else {
		vscode.window.showErrorMessage("ERROR: Something went wrong...");
	}
};

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export const activate = (context: vscode.ExtensionContext) => {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	// console.log('Congratulations, your extension "hello-world" is now active!');

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
};

// this method is called when your extension is deactivated
export function deactivate() {}
