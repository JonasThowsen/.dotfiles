function tsdev --description "Manage local development services through Tailscale"
    set -l action $argv[1]

    switch "$action"
        case up
            if test (count $argv) -ne 3
                echo "Usage: tsdev up <service> <port>"
                return 1
            end

            set -l service (string replace -r '^svc:' '' -- $argv[2])
            set -l port $argv[3]

            if not string match -rq '^[a-z0-9][a-z0-9-]*$' -- "$service"
                echo "Invalid service name: $service"
                return 1
            end

            if not string match -rq '^[0-9]+$' -- "$port"
                echo "Invalid port: $port"
                return 1
            end

            if test "$port" -lt 1; or test "$port" -gt 65535
                echo "Port must be between 1 and 65535"
                return 1
            end

            tailscale serve \
                --service="svc:$service" \
                --https=443 \
                "127.0.0.1:$port"

            if test $status -ne 0
                return 1
            end

            tsdev url "$service"

        case down
            if test (count $argv) -ne 2
                echo "Usage: tsdev down <service>"
                return 1
            end

            set -l service (string replace -r '^svc:' '' -- $argv[2])

            if not string match -rq '^[a-z0-9][a-z0-9-]*$' -- "$service"
                echo "Invalid service name: $service"
                return 1
            end

            tailscale serve drain "svc:$service"
            and tailscale serve clear "svc:$service"

        case url
            if test (count $argv) -ne 2
                echo "Usage: tsdev url <service>"
                return 1
            end

            set -l service (string replace -r '^svc:' '' -- $argv[2])

            if not string match -rq '^[a-z0-9][a-z0-9-]*$' -- "$service"
                echo "Invalid service name: $service"
                return 1
            end

            if not command -q jq
                echo "tsdev requires jq to determine the tailnet DNS suffix"
                return 1
            end

            set -l suffix (tailscale status --json | jq -r '.MagicDNSSuffix')

            if test -z "$suffix"; or test "$suffix" = "null"
                echo "Could not determine the tailnet DNS suffix"
                return 1
            end

            echo "https://$service.$suffix/"

        case status
            if test (count $argv) -ne 1
                echo "Usage: tsdev status"
                return 1
            end

            if not command -q jq
                tailscale serve status --json
                return $status
            end

            tailscale serve status --json | jq -r '
                def without_default_https_port: sub(":443$"; "");

                if ((.Services // {}) | length) == 0 then
                    "No Tailscale Services configured."
                else
                    "Tailscale Services:",
                    (.Services | to_entries[] |
                        (.key | sub("^svc:"; "")) as $service |
                        (.value.Web // {} | to_entries[] |
                            "  \($service)\n    https://\(.key | without_default_https_port)/ -> \(.value.Handlers["/"].Proxy // "configured")"
                        )
                    )
                end,

                if ((.Foreground // {}) | length) > 0 then
                    "\nForeground device Serve sessions:",
                    (.Foreground | to_entries[] |
                        (.value.Web // {} | to_entries[] |
                            "  https://\(.key | without_default_https_port)/ -> \(.value.Handlers["/"].Proxy // "configured")"
                        )
                    )
                else
                    empty
                end
            '

        case '*'
            echo "Usage:"
            echo "  tsdev up <service> <port>"
            echo "  tsdev down <service>"
            echo "  tsdev url <service>"
            echo "  tsdev status"
            return 1
    end
end
