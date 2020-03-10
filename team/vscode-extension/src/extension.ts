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

const execIntellisense = (editor: vscode.TextEditor, diagcol: vscode.DiagnosticCollection) => {
	const code = `\"${editor.document.getText()}\"`;
	cp.exec(`verishot --intellisense ${code}`, (err: any, stdout: any, stderr: any) => {
		if (stdout && stdout.length) {
			console.log(stdout);
			let splitted = stdout.split(";");
			if (splitted.length >= 3) {
				let line: number = parseInt(splitted[0]);
				let char: number = parseInt(splitted[1]);
				let tooltipErr: string = splitted[2];
				const diagnostics: vscode.Diagnostic[] =
					[{
						severity: vscode.DiagnosticSeverity.Error,
						range: new vscode.Range(line, char, line+1, 0),
						message: tooltipErr,
					}];

				diagcol.set(editor.document.uri, diagnostics);
			}
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


	// INTELLISENSE
	let timeout: NodeJS.Timer | undefined = undefined;
	const diagnosticCollection = vscode.languages.createDiagnosticCollection();
	
	const triggerIntellisense = (activeEditor: vscode.TextEditor) => {
		if (timeout) {
			clearTimeout(timeout);
			timeout = undefined;
		}
		timeout = setTimeout(() => execIntellisense(activeEditor, diagnosticCollection), 1000);
	};

	vscode.window.onDidChangeActiveTextEditor((editor) => {
		if (editor) {
			triggerIntellisense(editor);
		}
	}, null, context.subscriptions);

	vscode.workspace.onDidChangeTextDocument((event) => {
		const editor = vscode.window.activeTextEditor;
		if (editor && event.document === editor.document) {
			triggerIntellisense(editor);
		}
	}, null, context.subscriptions);
};

// this method is called when your extension is deactivated
export function deactivate() {}
