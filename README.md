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
clamd:stream("X5O!P%@AP[4\\PZX54(P^)7CC)7}"),
clamd:stream("$EICAR-STANDARD-ANTIVIRUS-TEST-FILE!$H+H*"),
clamd:stream(""). % You end a stream with an empty string
```

## Features and todo

 * √ Talking to clamd
 * _ One session per stream
 * _ Connection pool