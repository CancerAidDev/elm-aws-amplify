import { Elm } from "./Main.elm";

const crypto = window.crypto || window.msCrypto;

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};

const randInts = getRandomInts(5);
const flags = {
  seed: [randInts[0], randInts.slice(1)],
  appId: process.env.APP_ID,
  identityPoolId: process.env.IDENTITY_POOL_ID,
  region: process.env.AWS_REGION,
  clientInfo: {
    platform: "platform",
    make: "make",
    model: "model",
    version: "version",
    appVersion: "appVersion",
    language: "language",
    timezone: "timezone",
  },
};
Elm.Main.init({ node: document.querySelector("main"), flags: flags });
