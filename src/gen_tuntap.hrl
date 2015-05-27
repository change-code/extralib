-record(tuntap, {
	port :: port(),
	mode :: tun |tap,
	ifname :: string(),
	state :: term()}).

-type option() :: {mode, tun | tap}
	| {ifname, string()}
	| term().

