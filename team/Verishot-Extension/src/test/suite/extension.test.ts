import * as assert from 'assert';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
import { checkFilePath } from '../../utility';

suite('Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.');

	test('checkFilePathTests', () => {
		assert.equal(checkFilePath(undefined, true), false);
		assert.equal(checkFilePath("ers/fdaf/fdar.v", true), true);
		assert.equal(checkFilePath("fdasf.fdfad.vv", true), false);
	});
});
