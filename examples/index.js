import { clientInfo } from "./browser";
import { Elm } from "./Main.elm";

const crypto = window.crypto || window.msCrypto;

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};

const randInts = getRandomInts(5);
const flags = {
  date: new Date().toISOString(),
  seed: [randInts[0], randInts.slice(1)],
  pinpointProjectId: process.env.PINPOINT_PROJECT_ID,
  identityPoolId: process.env.IDENTITY_POOL_ID,
  region: process.env.AWS_REGION,
  clientInfo: clientInfo(),
};

Elm.Main.init({ node: document.querySelector("main"), flags: flags });
