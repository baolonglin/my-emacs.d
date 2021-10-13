function FindProxyForURL(url, host) {
	if (isInNet(host, "10.117.146.224", "255.255.255.224") || shExpMatch(host, "*.ericsson.se"))
	{
		return "SOCKS 127.0.0.1:9996";
	}
	return "DIRECT";
}
