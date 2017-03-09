FROM erlang:18

COPY . /usr/app

WORKDIR /usr/app

RUN make release

CMD ["make", "run"]
