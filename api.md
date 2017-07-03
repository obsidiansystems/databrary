# github.com/databrary/databrary

Databrary 2.0 API

## Routes

<details>
<summary>`/`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/**
	- _GET_
		- [main.main.func1](/databrary.go#L101)

</details>
<details>
<summary>`/api/autocomplete-affil`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/autocomplete-affil**
		- _GET_
			- [AutoCompleteAffil](/routes/api.go#L61)

</details>
<details>
<summary>`/api/loggedin`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/loggedin**
		- _GET_
			- [IsLoggedInEndpoint](/routes/user.go#L187)

</details>
<details>
<summary>`/api/report-error`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/report-error**
		- _POST_
			- [ReportError](/routes/api.go#L121)

</details>
<details>
<summary>`/api/user/check-token`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/check-token**
			- _POST_
				- [CheckTokenExpiryEndpoint](/routes/user.go#L290)

</details>
<details>
<summary>`/api/user/exists`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/exists**
			- _GET_
				- [UserExists](/routes/user.go#L476)

</details>
<details>
<summary>`/api/user/login`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/login**
			- _POST_
				- [PostLogin](/routes/user.go#L28)
			- _GET_
				- [GetLogin](/routes/static.go#L12)

</details>
<details>
<summary>`/api/user/logout`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/logout**
			- _POST_
				- [PostLogOut](/routes/user.go#L117)

</details>
<details>
<summary>`/api/user/profile`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/profile**
			- **/**
				- _GET_
					- [GetProfile](/routes/user.go#L648)
				- _PATCH_
					- [PatchProfile](/routes/user.go#L703)

</details>
<details>
<summary>`/api/user/register`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/register**
			- _POST_
				- [Register](/routes/user.go#L522)

</details>
<details>
<summary>`/api/user/reset-password/email`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/reset-password/email**
			- _POST_
				- [ResetPasswordEmail](/routes/user.go#L208)

</details>
<details>
<summary>`/api/user/reset-password/token`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/api**
	- **/user**
		- **/reset-password/token**
			- _POST_
				- [ResetPasswordToken](/routes/user.go#L353)

</details>
<details>
<summary>`/public`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/public**
	- _GET_
		- [(Handler).ServeHTTP-fm](https:///usr/local/go/src/net/http/h2_bundle.go#L4331)

</details>
<details>
<summary>`/public/*`</summary>

- [RequestID](/vendor/github.com/pressly/chi/middleware/request_id.go#L63)
- [RealIP](/vendor/github.com/pressly/chi/middleware/realip.go#L29)
- [RequestLogger.func1](/vendor/github.com/pressly/chi/middleware/logger.go#L31)
- [Recoverer](/vendor/github.com/pressly/chi/middleware/recoverer.go#L16)
- [github.com/databrary/databrary/vendor/github.com/unrolled/secure.(*Secure).Handler-fm](/databrary.go#L78)
- [Timeout.func1](/vendor/github.com/pressly/chi/middleware/timeout.go#L33)
- [github.com/databrary/databrary/vendor/gopkg.in/throttled/throttled%2ev2.(*HTTPRateLimiter).RateLimit-fm](/databrary.go#L86)
- [github.com/databrary/databrary/vendor/github.com/rs/cors.(*Cors).Handler-fm](/databrary.go#L95)
- [Manage.func1](/vendor/github.com/databrary/scs/session/manager.go#L36)
- [StripSlashes](/vendor/github.com/pressly/chi/middleware/strip.go#L12)
- **/public/***
	- _GET_
		- [(*Mux).FileServer.func1](/vendor/github.com/pressly/chi/mux.go#L317)

</details>

Total # of routes: 14
