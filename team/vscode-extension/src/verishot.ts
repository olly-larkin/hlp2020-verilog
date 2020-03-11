import * as vscode from 'vscode';
import { checkFilePath, VerishotMode } from './utility';

export const execVerishot = (verishotMode: number, kwargs: Object = {}) => {
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
		vscode.window.showInformationMessage('Linting...');
		terminal.sendText(`verishot --lint ${filePath}`);
	}
	else if (verishotMode === VerishotMode.simulate) {
		vscode.window.showInformationMessage('Simulating...');
		terminal.sendText(`verishot --simulate ${filePath} ${workspacePath}`);
	}
	else if (verishotMode === VerishotMode.visualise) {
		vscode.window.showInformationMessage('Visualising...');
		terminal.sendText(`verishot --visualise ${filePath} ${workspacePath}`);
	}
	else {
		vscode.window.showErrorMessage("ERROR: Something went wrong...");
	}
};

