const express = require("express");
const app = express();
const port = 4002;

app.get("/json", (req, res) => {
  res.setHeader("Content-Type", "application/json");

  res.json({
    ip: "92.159.35.57",
    hostname: "aaubervilliers-198-1-68-57.w92-159.abo.wanadoo.fr",
    city: "Saint-André-lez-Lille",
    region: "Hauts-de-France",
    country: "FR",
    loc: "50.6667,3.0500",
    org: "AS3215 Orange S.A.",
    postal: "59350",
    timezone: "Europe/Paris",
    readme: "https://ipinfo.io/missingauth",
  });
});

app.listen(port, () => {
  console.log(`IP api ready at http://localhost:${port}`);
});
