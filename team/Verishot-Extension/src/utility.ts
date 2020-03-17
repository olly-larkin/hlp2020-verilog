import * as vscode from 'vscode';
import * as fs from 'fs';
import * as os from 'os';
import * as cp from 'child_process';

export enum VerishotMode {
	lint,
	simulate,
	visualise,
}

export const dirSlash = os.platform() === 'win32' ? '\\' : '/';

export const getFileExtension = (fileName: string): string => {
	return fileName.substring(fileName.lastIndexOf('.') + 1, fileName.length) || fileName;
};

export const stripFileExtension = (fileName: string): string => {
	return fileName.substring(0, fileName.lastIndexOf('.')) || fileName;
};

// check whether a .vproj file exists in the current file system
export const getNumberOfProjectFiles = (workspacePath: string) => {
	return fs.readdirSync(workspacePath).filter((file) => getFileExtension(file) === 'vproj').length;
};

export const getProjectName = (workspacePath: string) => {
	const numberOfProjectFiles = getNumberOfProjectFiles(workspacePath);
	if (numberOfProjectFiles === 1) {
		return fs.readdirSync(workspacePath).filter((file) => getFileExtension(file) === 'vproj')[0].slice(0, -6);
	}
	else if (!numberOfProjectFiles) {
		vscode.window.showErrorMessage(`Create a new project first`);
	}
	else {
		vscode.window.showErrorMessage(`More than one project file exists`);
	}
	return undefined;
};

export const getWorkspacePath = () => {
	const folders = vscode.workspace.workspaceFolders || [];
	if (folders.length === 1) {
		return folders[0].uri.fsPath;
	}
	else {
        vscode.window.showErrorMessage(`Verishot only supports 1 project folder.`);
	}
	return undefined;
};

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

// if a terminal with the required terminal name already exists, use that terminal
export const getTerminal = (terminalName: string) => {
	const terms = vscode.window.terminals.filter((term: vscode.Terminal) => term.name === terminalName);
	if (terms.length) { return terms[0]; }
	return vscode.window.createTerminal(terminalName);
};

export const spawnCmdWithFeedback = (binName: string, args: string[]) => {
	const s = cp.spawnSync(binName, args);
	const stderrText = s.stderr.toString().trim();
	const stdoutText = s.stdout.toString().trim();
	if (stderrText) {
		vscode.window.showErrorMessage(stderrText);
	}
	else {
		const outfunc = s.status === 0 ? vscode.window.showInformationMessage : vscode.window.showErrorMessage;
		outfunc(stdoutText);
	}
};

export const getExistingModules = (workspacePath: string, projectName: string): string[] | undefined => {
	const s = cp.spawnSync(`verishot`, [workspacePath, projectName]);
	const stderrText = s.stderr.toString().trim();
	const stdoutText = s.stdout.toString().trim();
	if (stderrText) {
		vscode.window.showErrorMessage(stderrText);
		vscode.window.showErrorMessage(`Something went wrong`);
		return undefined;
	}
	else {
		return stdoutText
			.split(`\n`)
			.map((line: string) => { return stripFileExtension(line.trim()); });
	}
};