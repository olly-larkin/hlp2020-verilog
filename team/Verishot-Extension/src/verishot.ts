import * as vscode from 'vscode';
import { checkFilePath, VerishotMode, getWorkspacePath, getProjectName, dirSlash, spawnCmdWithFeedback } from './utility';

const spawnCmdWithMsg = (msg: string, args: string[]) => {
	vscode.window.setStatusBarMessage(msg,
		new Promise((resolve, reject) => {
			spawnCmdWithFeedback(`verishot`, args);
			resolve();
		})
	);
};

export const execVerishot = (verishotMode: number, kwargs: Object = {}) => {
	const workspacePath = getWorkspacePath();
	if (!workspacePath) { return; }


	if (verishotMode === VerishotMode.lint) {
		const filePath = vscode.window.activeTextEditor?.document.fileName || "";
		if (!checkFilePath(filePath, false)) { return; }
		const statusMsg = `Linting...`;
		const args: string[] = [`--lint`, filePath];
		spawnCmdWithMsg(statusMsg, args);
	}
	else if (verishotMode === VerishotMode.simulate) {
		const projectName = getProjectName(workspacePath);
		if (!projectName) { return; }
		const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;
		const statusMsg = `Simulating...`;
		const args: string[] = [`--simulate`, vprojFilePath];
		spawnCmdWithMsg(statusMsg, args);
	}
	else if (verishotMode === VerishotMode.visualise) {
		const projectName = getProjectName(workspacePath);
		if (!projectName) { return; }
		const vprojFilePath = `${workspacePath}${dirSlash}${projectName}.vproj`;
		const statusMsg = `Visualising...`;
		const args: string[] = [`--visualise`, vprojFilePath];
		spawnCmdWithMsg(statusMsg, args);
	}
	else {
		vscode.window.showErrorMessage(`Something went wrong...`);
	}
};

