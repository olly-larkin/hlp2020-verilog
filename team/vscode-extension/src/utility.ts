import * as vscode from 'vscode';
import * as fs from 'fs';
import * as os from 'os';

export enum VerishotMode {
	lint,
	simulate,
	visualise,
}

export const dirSlash = os.platform() === 'win32' ? '\\' : '/';

export const getFileExtension = (fileName: string): string => {
	return fileName.substring(fileName.lastIndexOf('.')+1, fileName.length) || fileName;
};

// check whether a .vproj file exists in the current file system
export const getNumberOfProjectFiles = (workspacePath: string) => {
    return fs.readdirSync(workspacePath).filter((file) => getFileExtension(file) === 'vproj').length;
}

export const getProjectName = (workspacePath: string) => {
	if (getNumberOfProjectFiles(workspacePath) === 1) {
		return fs.readdirSync(workspacePath).filter((file) => getFileExtension(file) === 'vproj')[0].slice(0, -6);
	}
	return undefined;
}

export const checkFilePath = (filePath: string | undefined, suppress: boolean): boolean => {
	if (!filePath) {
		if (!suppress) { vscode.window.showErrorMessage("No active file found. Please open a `.v` Verilog file"); }
		return false;
	}
	else if (getFileExtension(filePath) !== "v") {
		if (!suppress) { vscode.window.showErrorMessage("Only `.v` Verilog files are allowed"); }
		return false;
	}
	return true;
};