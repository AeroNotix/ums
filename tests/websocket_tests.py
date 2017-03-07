import websocket
import unittest


class UMSWebsocketClientTests(unittest.TestCase):
    def test_can_connect(self):
        ws = websocket.WebSocket()
        ws.connect("ws://localhost:5564/subscribe")
        self.assertTrue(ws.connected)

    def test_websocket_has_session_header(self):
        ws = websocket.WebSocket()
        ws.connect("ws://localhost:5564/subscribe")
        self.assertTrue(ws.connected)
        self.assertIsNotNone(ws.headers.get('x-ums-session-id'))
