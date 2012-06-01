-record(subreq, {
    queue = erlang:error({required, queue}),
    ack,
    offset
}).

-record(pubresp, {
    topic = erlang:error({required, topic}),
    success = erlang:error({required, success}),
    offset
}).

-record(payload, {
    content_type = erlang:error({required, content_type}),
    content = erlang:error({required, content})
}).

