#!/usr/bin/env node

import { run } from "./feed";
import { parseOptions, usage } from "./options";

const main = async () => {
  const options = parseOptions(process.argv.slice(2));
  if (!options.ok) {
    console.error(options.error);
    console.error(usage);
    process.exit(1);
  }
  if (options.value.help) {
    console.log(usage);
    process.exit(0);
  }
  run(options.value);
}

main().catch((error) => {
  console.error("Error:", error.message);
  process.exit(1);
});
