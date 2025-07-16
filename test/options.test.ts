import assert from 'node:assert/strict';
import { describe, it } from 'node:test';
import { parseOptions } from '../src/options';

describe('parseOptions', () => {
  it('no URL', () => {
    const options = parseOptions([]);
    assert.strictEqual(options.ok, false);
    assert.strictEqual(options.error, '');
  });

  it ('URL', () => {
    const url = 'https://www.example.com/';
    const options = parseOptions([url]);
    assert.strictEqual(options.ok, true);
    assert.strictEqual(options.value.check, false);
    assert.strictEqual(options.value.guess, false);
    assert.strictEqual(options.value.help, false);
    assert.strictEqual(options.value.url, url);
  });

  it('multiple URLs', () => {
    const url = 'https://www.example.com/';
    const options = parseOptions([`${url}/1`, `${url}/2`]);
    assert.strictEqual(options.ok, false);
    assert.strictEqual(options.error, '');
  });
});
