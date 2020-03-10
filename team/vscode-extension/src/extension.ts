// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as cp from 'child_process';

enum VerishotMode {
	lint,
	simulate,
	visualise,
}

const checkFilePath = (filePath: string | undefined, suppress: boolean): boolean => {
	if (!filePath) {
		if (!suppress) { vscode.window.showErrorMessage("ERROR: No active file found. Please open a `.v` Verilog file"); }
		return false;
	}
	else if (filePath.substr(filePath.length - 2) !== ".v") {
		if (!suppress) { vscode.window.showErrorMessage("ERROR: Only `.v` Verilog files are allowed"); }
		return false;
	}
	return true;
};


const execVerishot = (verishotMode: number) => {
	const filePath = vscode.window.activeTextEditor?.document.fileName;
	const workspacePath = vscode.workspace.rootPath;
	const terminalName = "Verishot";

	// if a terminal with the required terminal name already exists, use that terminal
	const getTerminal = (terminalName: string) => {
		const terms = vscode.window.terminals.filter((term: vscode.Terminal) => term.name === terminalName);
		if (terms.length) { return terms[0]; }
		return vscode.window.createTerminal(terminalName);
	};
	const terminal = getTerminal(terminalName);

	if (!checkFilePath(filePath, false)) {
		return;
	}
	terminal.show();
	if (verishotMode === VerishotMode.lint) {
		terminal.sendText(`verishot --lint ${filePath}`);
	}
	else if (verishotMode === VerishotMode.simulate) {
		terminal.sendText(`verishot --simulate ${filePath} ${workspacePath}`);
	}
	else if (verishotMode === VerishotMode.visualise) {
		terminal.sendText(`verishot --visualise ${filePath} ${workspacePath}`);
	}
	else {
		vscode.window.showErrorMessage("ERROR: Something went wrong...");
	}
};

const execIntellisense = () => {
	const filePath = vscode.window.activeTextEditor?.document.fileName;
	cp.exec(`verishot --intellisense ${filePath}`, (err: any, stdout: any, stderr: any) => {
		if (stdout && stdout.length) {
			console.log(stdout);
		}
	});
};

export const activate = (context: vscode.ExtensionContext) => {
	console.log("[VERISHOT ACTIVATED]");
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

	// intellisense on save
	vscode.workspace.onDidSaveTextDocument((event: vscode.TextDocument) => {
		if (checkFilePath(event.fileName, true)) {
			execIntellisense();
		};
	});


};

// this method is called when your extension is deactivated
export function deactivate() {}
