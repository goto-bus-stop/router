FROM node:18-bullseye-slim

WORKDIR /usr/src/app

COPY package.json package-lock.json ./

RUN npm ci

COPY weather.js .

USER node

CMD [ "node", "weather.js" ]
