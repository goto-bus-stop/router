const express = require("express");
const app = express();

const cors = require("cors");
const rateLimit = require("express-rate-limit");

const port = process.env.PORT || 4002;

const rateLimitTreshold = process.env.LIMIT || 5000;

const limiter = rateLimit({
  windowMs: 60 * 60 * 1000, // 1 hour
  max: rateLimitTreshold,
});

app.use("/", cors(), limiter);

app.get("/json", (req, res) => {
  res.setHeader("Content-Type", "application/json");

  res.json({
    ip: "1.2.3.4",
    hostname: "this.is.a.test.hostname.fr",
    city: "Saint-André-lez-Lille",
    region: "Hauts-de-France",
    country: "FR",
    loc: "12.34,56.78",
    org: "AS12345 Orange S.A.",
    postal: "12345",
    timezone: "Europe/Paris",
    readme: "https://ipinfo.io/missingauth",
  });
});

app.listen(port, () => {
  console.log(`IP api ready at http://localhost:${port}`);
});
