FROM node:18-bullseye-slim

WORKDIR /usr/src/app

COPY package.json .

RUN npm install

COPY ip.js .

CMD [ "node", "ip.js" ]
