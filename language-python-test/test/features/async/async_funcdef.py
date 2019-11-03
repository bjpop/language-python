import asyncio

@decorator.decorator
async def http_get(domain):
    reader, writer = await asyncio.open_connection(domain, 80)
    writer.close()

loop = asyncio.get_event_loop()
try:
    loop.run_until_complete(http_get('example.com'))
finally:
    loop.close()
