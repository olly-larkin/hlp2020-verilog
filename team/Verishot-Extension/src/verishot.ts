import * as vscode from 'vscode';
import { getExistingModuleFileNamesFromVProj, checkFilePath, VerishotMode, getWorkspacePath, getTerminal, getProjectName, dirSlash } from './utility';
import * as fs from 'fs';
import * as cp from 'child_process';


const execCmd = (msg: string, args: string[]) => {
	vscode.window.setStatusBarMessage(msg,
		new Promise((resolve, reject) => {
			const s = cp.spawnSync('verishot', args);
			const stderrText = s.stderr.toString().trim();
			const stdoutText = s.stdout.toString().trim();
			if (stderrText) {
				vscode.window.showErrorMessage(stderrText);
			}
			else {
				const outfunc = s.status === 0 ? vscode.window.showInformationMessage : vscode.window.showErrorMessage;
				outfunc(stdoutText);
			}
			resolve();
		})
	);
};

/* sanity check before visualise and simulate */
/* NOTE: this is unneeded, verishot binary itself sanity checks */
export const vprojSanityCheck = (workspacePath: string, projectName: string): boolean => {
	return true;
	// const allModules = getExistingModuleFileNamesFromVProj(workspacePath, projectName);

	// return checkVProjFilesExists(allModules, workspacePath) &&
	// 	lintAllModules(allModules, workspacePath);
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
		const args = [`--lint`, filePath];
		const s = cp.spawnSync('verishot', args);
		const stderrText = s.stderr.toString().trim();
		const stdoutText = s.stdout.toString().trim();
		if (stderrText) {
			vscode.window.showErrorMessage(stderrText);
		}
		else if (!stdoutText.includes(`Verishot Lint: No Errors`)) {
			vscode.window.showErrorMessage(`ERROR in module \`${module}\`: ${stdoutText}`);
			return false;
		}
	}

	return true;
};

// const getAndSendTerminal = (terminalName: string, cmd: string) => {
// 	const terminal = getTerminal(terminalName);
// 	terminal.show();
// 	terminal.sendText(cmd);
// };

export const execVerishot = (verishotMode: number, kwargs: Object = {}) => {
	const workspacePath = getWorkspacePath();
	if (!workspacePath) { return; }


	if (verishotMode === VerishotMode.lint) {
		const filePath = vscode.window.activeTextEditor?.document.fileName || "";
		if (!checkFilePath(filePath, false)) { return; }
		const statusMsg = `Linting...`;
		const args: string[] = [`--lint`, filePath];
		execCmd(statusMsg, args);
	}
	else if (verishotMode === VerishotMode.simulate) {
		const projectName = getProjectName(workspacePath);
		if (!projectName) { return; }
		if (!vprojSanityCheck(workspacePath, projectName)) { return; }
		const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;
		const statusMsg = `Simulating...`;
		const args: string[] = [`--simulate`, vprojFilePath];
		execCmd(statusMsg, args);
	}
	else if (verishotMode === VerishotMode.visualise) {
		const projectName = getProjectName(workspacePath);
		if (!projectName) { return; }
		if (!vprojSanityCheck(workspacePath, projectName)) { return; }
		const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;
		const statusMsg = `Visualising...`;
		const args: string[] = [`--visualise`, vprojFilePath];
		execCmd(statusMsg, args);
	}
	else {
		vscode.window.showErrorMessage(`Something went wrong...`);
	}
};

