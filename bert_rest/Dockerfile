FROM python:3.8.5-slim-buster
MAINTAINER moiraghif <f.moiraghimotta@campus.unimib.it>

WORKDIR /usr/src/app
COPY . .

RUN pip install --no-cache-dir -r requirements.txt
EXPOSE 8080

CMD python bert_rest.py
