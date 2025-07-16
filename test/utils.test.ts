import assert from 'node:assert/strict';
import { beforeEach, describe, it } from 'node:test';
import { MockAgent, setGlobalDispatcher } from 'undici';
import { fetchWithUserAgent } from '../src/utils';

describe('fetchWithUserAgent', { concurrency: true }, () => {
  let agent: MockAgent<MockAgent.Options>;

  beforeEach(() => {
    agent = new MockAgent();
    setGlobalDispatcher(agent);
  });

  it('adds user agent', async () => {
    const url = 'https://www.example.com';
    const data = { key: 'intercepted' };
    agent
      .get(url)
      .intercept({
        headers: (headers) => {
          for (const [key, value] of Object.entries(headers)) {
            if (key === "user-agent" && value === "foxfeet/1.0.0") {
              return true;
            }
          }
          return false;
        },
        method: (method) => true,
        path: (path) => true,
      })
      .reply(200, data);
    const response = await fetchWithUserAgent(url);
    assert.deepEqual(await response.json(), data);
  });
});
