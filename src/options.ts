import { parseArgs } from "node:util";
import { Result } from "./utils";

export interface Options {
  check: boolean;
  guess: boolean;
  help: boolean;
  url: string;
};

export const parseOptions = (args: string[]): Result<Options> => {
  const { values, positionals } = parseArgs({
    allowPositionals: true,
    args,
    options: {
      help: {
        short: 'h',
        type: 'boolean',
        default: false,
      },
      check: {
        type: 'boolean',
        default: false,
      },
      guess: {
        type: 'boolean',
        default: false,
      },
    },
  });
  if (positionals.length === 1) {
    return { ok: true, value: { ...values, url: positionals[0] } };
  } else if (values.help) {
    return { ok: true, value: { ...values, url: '' } };
  } else {
    return { ok: false, error: "" };
  }
}

export const usage: string = `
Usage: foxfeet [options] <url>

Options:
  ${"--check".padEnd(25)}
  ${"--guess".padEnd(25)}
  ${"-h, --help".padEnd(25)}
`.trim();
