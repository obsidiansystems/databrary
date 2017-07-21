package routes

import (
	"gopkg.in/throttled/throttled.v2"
	"gopkg.in/throttled/throttled.v2/store/memstore"
)

// Throttle queries. Keeps record of ip.
func NewRateLimiter() (throttled.HTTPRateLimiter, error) {

	store, err := memstore.New(65536)
	if err != nil {
		return throttled.HTTPRateLimiter{}, err
	}

	quota := throttled.RateQuota{throttled.PerSec(2), 5}
	rateLimiter, err := throttled.NewGCRARateLimiter(store, quota)
	if err != nil {
		return throttled.HTTPRateLimiter{}, err
	}

	return throttled.HTTPRateLimiter{
		RateLimiter: rateLimiter,
		VaryBy:      &throttled.VaryBy{RemoteAddr: true},
	}, nil
}
