import websocket
import unittest


class UMSWebsocketClientTests(unittest.TestCase):
    def test_can_connect(self):
        ws = websocket.WebSocket()
        ws.connect("ws://localhost:5564/subscribe")
        self.assertTrue(ws.connected)
