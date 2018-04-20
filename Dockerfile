FROM ums-dependencies

COPY . /usr/app

WORKDIR /usr/app

RUN ./rebar3 as k8 release && \
    rm -rf /usr/app/_build/default/lib/ && \
    rm -rf /usr/app/_build/default/plugins/*/.git

ENV RELX_REPLACE_OS_VARS=true

CMD ["/usr/app/_build/k8/rel/ums/bin/ums", "foreground"]
