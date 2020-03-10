import * as vscode from 'vscode';
import { checkFilePath, VerishotMode } from './util';

export const execVerishot = (verishotMode: number) => {
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

