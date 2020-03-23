import * as vscode from 'vscode';
import * as fs from 'fs';
import * as cp from 'child_process';
import * as path from 'path';

export const binName = `verishot`;

export enum VerishotMode {
	lint,
	simulate,
	visualise,
}

export enum exitCodes {
	Success = 0,
	InvalidCmd = 1,
	NewProjectError = 2,
	NewModuleError = 3,
	DeleteModuleError = 4,
	SanityCheckError = 5,

	// LINT
	LintError = 10,

	// VISUALISE
	VisualisationError = 20,

	// SIMULATION
	SimulationError = 30,
	vInError = 31
}

// it is too spammy to split, so leave as joined for now.
export const showInformationMessageSeparated = (outText: string) => {
	vscode.window.showInformationMessage(outText);
	// outText
		// .split(`\n`)
		// .forEach((line: string) => vscode.window.showInformationMessage(line));
};

// it is too spammy to split, so leave as joined for now.
export const showErrorMessageSeparated = (outText: string) => {
	vscode.window.showErrorMessage(outText);
	// outText
	// 	.split(`\n`)
	// 	.forEach((line: string) => vscode.window.showErrorMessage(line));
};

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
		showErrorMessageSeparated(`Create a new project first`);
	}
	else {
		showErrorMessageSeparated(`More than one project file exists`);
	}
	return undefined;
};

export const getWorkspacePath = () => {
	const folders = vscode.workspace.workspaceFolders || [];
	if (folders.length === 1) {
		return folders[0].uri.fsPath;
	}
	else {
		showErrorMessageSeparated(`Verishot only supports 1 project folder.`);
	}
	return undefined;
};

export const checkFilePath = (filePath: string | undefined, suppress: boolean): boolean => {
	if (!filePath) {
		if (!suppress) { showErrorMessageSeparated("No active file found. Please open a `.v` Verilog file"); }
		return false;
	}
	else if (getFileExtension(filePath) !== "v") {
		if (!suppress) { showErrorMessageSeparated("Only `.v` Verilog files are allowed"); }
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

/* succFunc: function to run when succeed */
export const spawnCmdWithFeedback = (binName: string, args: string[], funcMap: Map<number, any> = new Map<number, any>()) => {
	const s = cp.spawnSync(binName, args);
	const stderrText = s.stderr.toString().trim();
	const stdoutText = s.stdout.toString().trim();
	if (stderrText) {
		showErrorMessageSeparated(stderrText);
	}
	else {
		const success = s.status === 0;
		const outFunc = success ? showInformationMessageSeparated : showErrorMessageSeparated;
		outFunc(stdoutText);
		let retStatus = s.status === null ? NaN : s.status;
		if (funcMap.has(retStatus)) {
			funcMap.get(retStatus)();
		}
	}
};

export const getExistingModules = (workspacePath: string, projectName: string): string[] | undefined => {
	const vprojFilePath = path.join(workspacePath, `${projectName}.vproj`);
	const s = cp.spawnSync(binName, [`--list-modules`, vprojFilePath]);
	const stderrText = s.stderr.toString().trim();
	const stdoutText = s.stdout.toString().trim();
	if (stderrText) {
		showErrorMessageSeparated(stderrText);
		showErrorMessageSeparated(`Something went wrong`);
		return undefined;
	}
	else {
		return stdoutText
			.split(`\n`)
			.map((line: string) => { return stripFileExtension(line.trim()); });
	}
};

/// ensure directory exists before writing file
export const ensureDirectoryExists = (filePath: string) => {
	const dirName = path.dirname(filePath);
	if (!fs.existsSync(dirName)) {
		fs.mkdirSync(dirName, { recursive: true });
	}
};