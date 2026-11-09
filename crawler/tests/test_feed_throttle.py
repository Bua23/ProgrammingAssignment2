import sys
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import ats_clients  # noqa: E402


def resp(status, payload=None):
    r = MagicMock(); r.status_code = status; r.headers = {"Retry-After": "49000"}
    r.json.return_value = payload; r.raise_for_status.side_effect = None if status < 400 else RuntimeError(status)
    return r


class FeedThrottleTest(unittest.TestCase):
    def setUp(self):
        ats_clients._blocked_hosts.clear(); ats_clients._host_last_request.clear()

    def test_429_blocks_host_for_rest_of_run_without_more_requests(self):
        with patch("ats_clients.requests.get", return_value=resp(429)) as get:
            with self.assertRaises(ats_clients.FeedRefused):
                ats_clients._get("https://apply.workable.com/api/v1/widget/accounts/a")
            with self.assertRaises(ats_clients.FeedRefused):
                ats_clients._get("https://apply.workable.com/api/v1/widget/accounts/b")
            self.assertEqual(get.call_count, 1)          # second company never hit the network

    def test_other_hosts_unaffected_by_a_block(self):
        ats_clients._blocked_hosts.add("apply.workable.com")
        with patch("ats_clients.requests.get", return_value=resp(200, {"jobs": []})):
            self.assertEqual(ats_clients._get("https://api.ashbyhq.com/posting-api/job-board/x"), {"jobs": []})

    def test_gated_host_waits_between_requests(self):
        with patch("ats_clients.requests.get", return_value=resp(200, {"jobs": []})), \
             patch("ats_clients.time.sleep") as sleep, \
             patch.dict(ats_clients.HOST_MIN_GAP_SECONDS, {"apply.workable.com": 12.0}):
            ats_clients._get("https://apply.workable.com/api/v1/widget/accounts/a")
            ats_clients._get("https://apply.workable.com/api/v1/widget/accounts/b")
            self.assertTrue(sleep.called)
            self.assertGreater(sleep.call_args[0][0], 0)


if __name__ == "__main__":
    unittest.main()
