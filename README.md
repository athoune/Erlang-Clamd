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

	./rebar compile eunit

### Example

```erlang
application:start(clamd),
{ok, _} = clamd:ping() %You can ping it
{ok, Pid} = clamd:open_stream(), %Open a worker
clamd_stream:chunk(Pid, "X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
clamd_stream:chunk(Pid, "$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
{ok, virus, Message} = clamd_stream:finish(Pid).
```

## Features and todo

 * √ Talking to clamd
 * _ One session per stream
 * _ Connection pool