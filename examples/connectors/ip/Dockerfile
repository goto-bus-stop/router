FROM node:18-bullseye-slim

WORKDIR /usr/src/app

COPY package.json package-lock.json ./

RUN npm ci

COPY ip.js .

USER node

CMD [ "node", "ip.js" ]
