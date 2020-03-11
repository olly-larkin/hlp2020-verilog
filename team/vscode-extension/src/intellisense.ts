import * as vscode from 'vscode';
import * as cp from 'child_process';
import { checkFilePath } from './utility';

export const execIntellisense = (editor: vscode.TextEditor, doc: vscode.TextDocument, diagcol: vscode.DiagnosticCollection) => {
	diagcol.clear();
	const filePath = doc.fileName;
	if (checkFilePath(filePath, true)) {
		cp.exec(`verishot --intellisense ${filePath}`, (err: any, stdout: any, stderr: any) => {
			if (stdout && stdout.length) {
				console.log(stdout);
				let splitted = stdout.split("#####");
				if (splitted.length === 3) {
					let line: number = parseInt(splitted[0])-1; // 1-indexed
					let char: number = parseInt(splitted[1]);
					let errMsg: string = splitted[2];
					const diagnostics: vscode.Diagnostic[] =
						[{
							severity: vscode.DiagnosticSeverity.Error,
							range: new vscode.Range(line, 0, line, char),
							message: errMsg,
						}];
					diagcol.set(editor.document.uri, diagnostics);
				}
			}
		});
	}
};