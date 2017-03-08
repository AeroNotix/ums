import time
import requests
import json
from contextlib import contextmanager
import websocket
import unittest


UMS_WS_ENDPOINT = "ws://localhost:5564/subscribe"

@contextmanager
def websocket_connection(connect_to=UMS_WS_ENDPOINT, ws_should_stay_open=True):
    ws = websocket.WebSocket()
    ws.connect(connect_to)
    yield ws
    assert ws.connected == ws_should_stay_open
    ws.close()


class UMSWebsocketClientTests(unittest.TestCase):

    def _simple_operation(self, ws, operation, extend={}):
        payload = {
            "op": operation,
            "resource": "00000-00000-00000-00000-00000",
            "edge": "http://localhost:5564/debug"
        }
        payload.update(extend)
        payload = json.dumps(payload)
        ws.send_binary(payload)
        self.assertEquals(
            json.loads((ws.recv()))["status"],
            "ok"
        )

    def simple_operation(self, operation, ws=None, extend={}):
        if ws is None:
            with websocket_connection() as ws:
                self._simple_operation(ws, operation, extend=extend)
        else:
            self._simple_operation(ws, operation, extend=extend)

    def test_websocket_can_connect(self):
        with websocket_connection() as ws:
            self.assertTrue(ws.connected)

    def test_websocket_has_session_header(self):
        with websocket_connection() as ws:
            self.assertIsNotNone(ws.headers.get('x-ums-session-id'))

    def test_websocket_can_subscribe_resources(self):
        self.simple_operation("subscribe")

    def test_websocket_can_unsubscribe_resources(self):
        self.simple_operation("unsubscribe")

    def test_websocket_reconnect_with_session_id(self):
        ws = websocket.WebSocket()
        ws.connect(UMS_WS_ENDPOINT)
        session_id = ws.headers['x-ums-session-id']
        self.simple_operation("subscribe", ws=ws)
        ws.close()
        ws.connect(UMS_WS_ENDPOINT, header={"x-ums-session-id": session_id})
        expected_reply = {
            "subscriptions": [{
                "resource": "00000-00000-00000-00000-00000",
                "edge": "http://localhost:5564/debug"
            }]
        }
        self.assertEquals(
            json.loads(ws.recv()),
            expected_reply
        )

    def test_can_route_message(self):
        with websocket_connection() as ws:
            self.simple_operation("subscribe", ws=ws)
            body = {
                "resources": ["00000-00000-00000-00000-00000"],
                "message": {
                    "random": "data",
                    "goes": "here"
                }
            }
            resp = requests.post("http://localhost:5564/route", json=body)
            self.assertEquals(resp.status_code, 200)
