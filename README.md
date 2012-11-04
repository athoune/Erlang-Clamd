# Clamd client for Erlang

Hunt virus with Erlang and [ClamAV](http://www.clamav.net/)

## Test it

### Install  clamav

On Linux

	sudo apt-get install clamav-daemon

On OSX

	brew install clamav

### Configuration

Open the TCP socket in `clamd.conf`

	TCPSocket 3310
	TCPAddr 127.0.0.1

Launch it. You can test it with `clamdscan`

On Linux

	sudo /etc/init.d/clamd start

On OSX

	sudo /usr/local/sbin/clamd

### Unit test

	./rebar eunit skip_deps=true

### Example

```erlang
application:start(clamd),
{ok, _} = clamd:ping(), %You can ping it
clamd:transaction(fun(Worker) ->
        clamd:start_stream(Worker),
        clamd:chunk_stream(Worker, "X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
        clamd:chunk_stream(Worker, "$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
        {ok, virus, "Eicar-Test-Signature"} = clamd:end_stream(Worker)
    end).
```

Clamd can be easily flooded, no more worker than CPU, so poolboy is used,
and each stream is handled inside a transaction.

## Features and todo

 * √ Talking to clamd
 * √ Connection pool
 * _ One session per stream
 * _ Some parameters


# Licence

MIT. © 2011, Mathieu Lecarme.
