const express = require("express");
const app = express();
const cors = require("cors");
const rateLimit = require("express-rate-limit");

const port = process.env.PORT || 4003;

const rateLimitTreshold = process.env.LIMIT || 5000;

const limiter = rateLimit({
  windowMs: 60 * 60 * 1000, // 1 hour
  max: rateLimitTreshold,
});

app.use("/", cors(), limiter);

app.get("/data/2.5/weather", (req, res) => {
  res.setHeader("Content-Type", "application/json");

  res.json({
    coord: {
      lon: 10.99,
      lat: 44.34,
    },
    weather: [
      {
        id: 501,
        main: "Rain",
        description: "moderate rain",
        icon: "10d",
      },
    ],
    base: "stations",
    main: {
      temp: 298.48,
      feels_like: 298.74,
      temp_min: 297.56,
      temp_max: 300.05,
      pressure: 1015,
      humidity: 64,
      sea_level: 1015,
      grnd_level: 933,
    },
    visibility: 10000,
    wind: {
      speed: 0.62,
      deg: 349,
      gust: 1.18,
    },
    rain: {
      "1h": 3.16,
    },
    clouds: {
      all: 100,
    },
    dt: 1661870592,
    sys: {
      type: 2,
      id: 2075663,
      country: "IT",
      sunrise: 1661834187,
      sunset: 1661882248,
    },
    timezone: 7200,
    id: 3163858,
    name: "Zocca",
    cod: 200,
  });
});

app.listen(port, () => {
  console.log(`Weather api ready at http://localhost:${port}`);
});
