FROM node:18-bullseye-slim

WORKDIR /usr/src/app

COPY package.json package-lock.json ./

RUN npm ci

COPY index.js .

USER node

CMD [ "npm", "run", "start" ]
