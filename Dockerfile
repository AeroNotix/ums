FROM ums-dependencies AS build-release

COPY . /usr/app

WORKDIR /usr/app

RUN ./rebar3 as k8 release && \
    rm -rf /usr/app/_build/default/lib/ && \
    rm -rf /usr/app/_build/default/plugins/*/.git

FROM erlang:23

COPY --from=build-release /usr/app/_build/k8/rel/ums /ums

ENV RELX_REPLACE_OS_VARS=true

CMD ["/ums/bin/ums", "foreground"]
