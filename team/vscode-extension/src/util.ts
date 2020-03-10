import * as vscode from 'vscode';

export enum VerishotMode {
	lint,
	simulate,
	visualise,
}


export const checkFilePath = (filePath: string | undefined, suppress: boolean): boolean => {
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