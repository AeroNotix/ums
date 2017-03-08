import requests
import json
from contextlib import contextmanager
import websocket
import unittest


RESOURCE = "00000-00000-00000-00000-00000"
UMS_DEBUG_ENDPOINT = "http://localhost:5564/debug"
UMS_SESSION_ID_HEADER = 'x-ums-session-id'
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
            "resource": RESOURCE,
            "edge": UMS_DEBUG_ENDPOINT
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
            self.assertIsNotNone(ws.headers.get(UMS_SESSION_ID_HEADER))

    def test_websocket_can_subscribe_resources(self):
        self.simple_operation("subscribe")

    def test_websocket_can_unsubscribe_resources(self):
        self.simple_operation("unsubscribe")

    def test_websocket_reconnect_with_session_id(self):
        ws = websocket.WebSocket()
        ws.connect(UMS_WS_ENDPOINT)
        session_id = ws.headers[UMS_SESSION_ID_HEADER]
        self.simple_operation("subscribe", ws=ws)
        ws.close()
        ws.connect(UMS_WS_ENDPOINT, header={"x-ums-session-id": session_id})
        expected_reply = {
            "subscriptions": [{
                "resource": RESOURCE,
                "edge": UMS_DEBUG_ENDPOINT
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
                "resources": [RESOURCE],
                "message": {
                    "random": "data",
                    "goes": "here"
                }
            }
            resp = requests.post(UMS_DEBUG_ENDPOINT, json=body)
            self.assertEquals(resp.status_code, 200)
            msg = ws.recv()
            self.assertEquals(
                json.loads(msg),
                {
                    "message": {
                        "random": "data",
                        "goes": "here"
                    }
                })
