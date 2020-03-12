import * as vscode from 'vscode';
import { getExistingModuleFileNamesFromVProj, checkFilePath, VerishotMode, getWorkspacePath, getTerminal, getProjectName, dirSlash } from './utility';
import * as fs from 'fs';
import * as cp from 'child_process';

/* sanity check before visualise and simulate */
export const vprojSanityCheck = (workspacePath: string, projectName: string): boolean => {
	const allModules = getExistingModuleFileNamesFromVProj(workspacePath, projectName);

	return checkVProjFilesExists(allModules, workspacePath) && 
		lintAllModules(allModules, workspacePath);
};

/* all .v files in .vproj exists */
export const checkVProjFilesExists = (allModules: Array<string>, workspacePath: string): boolean => {
	for (const module of allModules) {
		const moduleFilePath = `${workspacePath}${dirSlash}${module}`;
		if (!fs.existsSync(moduleFilePath)) { 
			vscode.window.showErrorMessage(`Module \`${module}\` does not exist. Use \`Verishot: Delete Module\` to properly delete modules`);
			return false; 
		}
	}
	
	return true;
};

/* lint all files and make sure there are no mistakes */ 
export const lintAllModules = (allModules: Array<string>, workspacePath: string) => {
	for (const module of allModules) {
		const filePath = `${workspacePath}${dirSlash}${module}`;
		try {
			// `|| echo 0` to prevent false throwing
			const stdout = cp.execSync(`verishot --lint ${filePath} || echo 0`).toString();
			if (!stdout.includes(`Verishot Lint: No Errors`)) {
				vscode.window.showErrorMessage(stdout);
				vscode.window.showErrorMessage(`Lint error for module \`${module}\``);
				return false;
			}
		}
		catch (err) {
			vscode.window.showErrorMessage(err.message);
		}
	}

	return true;
};

const getAndSendTerminal = (terminalName: string, cmd: string) => {
	const terminal = getTerminal(terminalName);
	terminal.show();
	terminal.sendText(cmd);
};

export const execVerishot = (verishotMode: number, kwargs: Object = {}) => {
	const workspacePath = getWorkspacePath();
	if (!workspacePath) { return; }

	const terminalName = "Verishot";


	if (verishotMode === VerishotMode.lint) {
		const filePath = vscode.window.activeTextEditor?.document.fileName;
		if (!checkFilePath(filePath, false)) { return; }
		vscode.window.showInformationMessage('Linting...');

		getAndSendTerminal(terminalName, `verishot --lint ${filePath}`);
	}
	else if (verishotMode === VerishotMode.simulate) {
		const projectName = getProjectName(workspacePath);
		if (!projectName) { return; }
		if (!vprojSanityCheck(workspacePath, projectName)) { return; }
		const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;

		vscode.window.showInformationMessage('Simulating...');

		getAndSendTerminal(terminalName, `verishot --simulate ${vprojFilePath}`);

	}
	else if (verishotMode === VerishotMode.visualise) {
		const projectName = getProjectName(workspacePath);
		if (!projectName) { return; }
		if (!vprojSanityCheck(workspacePath, projectName)) { return; }
		const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;

		vscode.window.showInformationMessage('Visualising...');

		getAndSendTerminal(terminalName, `verishot --visualise ${vprojFilePath}`);
	}
	else {
		vscode.window.showErrorMessage(`Something went wrong...`);
	}
};

