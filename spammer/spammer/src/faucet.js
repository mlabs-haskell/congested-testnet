const http = require('http');

const PORT = 8000;

const server = http.createServer((req, res) => {
    if (req.method === 'POST' && req.headers['content-type'] === 'application/json') {
        let body = '';

        req.on('data', chunk => {
            body += chunk;
        });

        req.on('end', () => {
            try {
                const data = JSON.parse(body);
                const pubKeyHashHex = data.pubKeyHashHex;

                if (pubKeyHashHex) {
                    res.writeHead(200, { 'Content-Type': 'application/json' });
                    res.end(JSON.stringify({ message: `Received pubKeyHashHex: ${pubKeyHashHex}` }));
                } else {
                    res.writeHead(400, { 'Content-Type': 'application/json' });
                    res.end(JSON.stringify({ error: 'pubKeyHashHex field is required' }));
                }
            } catch (err) {
                res.writeHead(400, { 'Content-Type': 'application/json' });
                res.end(JSON.stringify({ error: 'Invalid JSON' }));
            }
        });
    } else {
        res.writeHead(404, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ error: 'Not Found' }));
    }
});

server.listen(PORT, () => {
    console.log(`Server is running on port ${PORT}`);
});
