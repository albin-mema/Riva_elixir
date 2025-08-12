--
-- PostgreSQL database dump
--

-- Dumped from database version 15.13
-- Dumped by pg_dump version 17.5 (Ubuntu 17.5-0ubuntu0.25.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: citext; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS citext WITH SCHEMA public;


--
-- Name: EXTENSION citext; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION citext IS 'data type for case-insensitive character strings';


--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: ash_elixir_and(anycompatible, anycompatible); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.ash_elixir_and("left" anycompatible, "right" anycompatible, OUT f1 anycompatible) RETURNS anycompatible
    LANGUAGE sql IMMUTABLE
    SET search_path TO ''
    AS $_$
  SELECT CASE
    WHEN $1 IS NOT NULL THEN $2
    ELSE $1
  END $_$;


--
-- Name: ash_elixir_and(boolean, anycompatible); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.ash_elixir_and("left" boolean, "right" anycompatible, OUT f1 anycompatible) RETURNS anycompatible
    LANGUAGE sql IMMUTABLE
    SET search_path TO ''
    AS $_$
  SELECT CASE
    WHEN $1 IS TRUE THEN $2
    ELSE $1
  END $_$;


--
-- Name: ash_elixir_or(anycompatible, anycompatible); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.ash_elixir_or("left" anycompatible, "right" anycompatible, OUT f1 anycompatible) RETURNS anycompatible
    LANGUAGE sql IMMUTABLE
    SET search_path TO ''
    AS $_$ SELECT COALESCE($1, $2) $_$;


--
-- Name: ash_elixir_or(boolean, anycompatible); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.ash_elixir_or("left" boolean, "right" anycompatible, OUT f1 anycompatible) RETURNS anycompatible
    LANGUAGE sql IMMUTABLE
    SET search_path TO ''
    AS $_$ SELECT COALESCE(NULLIF($1, FALSE), $2) $_$;


--
-- Name: ash_raise_error(jsonb); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.ash_raise_error(json_data jsonb) RETURNS boolean
    LANGUAGE plpgsql STABLE
    SET search_path TO ''
    AS $$
BEGIN
    -- Raise an error with the provided JSON data.
    -- The JSON object is converted to text for inclusion in the error message.
    RAISE EXCEPTION 'ash_error: %', json_data::text;
    RETURN NULL;
END;
$$;


--
-- Name: ash_raise_error(jsonb, anycompatible); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.ash_raise_error(json_data jsonb, type_signal anycompatible) RETURNS anycompatible
    LANGUAGE plpgsql STABLE
    SET search_path TO ''
    AS $$
BEGIN
    -- Raise an error with the provided JSON data.
    -- The JSON object is converted to text for inclusion in the error message.
    RAISE EXCEPTION 'ash_error: %', json_data::text;
    RETURN NULL;
END;
$$;


--
-- Name: ash_trim_whitespace(text[]); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.ash_trim_whitespace(arr text[]) RETURNS text[]
    LANGUAGE plpgsql IMMUTABLE
    SET search_path TO ''
    AS $$
DECLARE
    start_index INT = 1;
    end_index INT = array_length(arr, 1);
BEGIN
    WHILE start_index <= end_index AND arr[start_index] = '' LOOP
        start_index := start_index + 1;
    END LOOP;

    WHILE end_index >= start_index AND arr[end_index] = '' LOOP
        end_index := end_index - 1;
    END LOOP;

    IF start_index > end_index THEN
        RETURN ARRAY[]::text[];
    ELSE
        RETURN arr[start_index : end_index];
    END IF;
END; $$;


--
-- Name: timestamp_from_uuid_v7(uuid); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.timestamp_from_uuid_v7(_uuid uuid) RETURNS timestamp without time zone
    LANGUAGE sql IMMUTABLE STRICT PARALLEL SAFE
    SET search_path TO ''
    AS $$
  SELECT to_timestamp(('x0000' || substr(_uuid::TEXT, 1, 8) || substr(_uuid::TEXT, 10, 4))::BIT(64)::BIGINT::NUMERIC / 1000);
$$;


--
-- Name: uuid_generate_v7(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.uuid_generate_v7() RETURNS uuid
    LANGUAGE plpgsql
    SET search_path TO ''
    AS $$
DECLARE
  timestamp    TIMESTAMPTZ;
  microseconds INT;
BEGIN
  timestamp    = clock_timestamp();
  microseconds = (cast(extract(microseconds FROM timestamp)::INT - (floor(extract(milliseconds FROM timestamp))::INT * 1000) AS DOUBLE PRECISION) * 4.096)::INT;

  RETURN encode(
    set_byte(
      set_byte(
        overlay(uuid_send(gen_random_uuid()) placing substring(int8send(floor(extract(epoch FROM timestamp) * 1000)::BIGINT) FROM 3) FROM 1 FOR 6
      ),
      6, (b'0111' || (microseconds >> 8)::bit(4))::bit(8)::int
    ),
    7, microseconds::bit(8)::int
  ),
  'hex')::UUID;
END
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: availability_exceptions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.availability_exceptions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    date date NOT NULL,
    start_time time(0) without time zone,
    end_time time(0) without time zone,
    is_available boolean DEFAULT false NOT NULL,
    reason text,
    exception_type text DEFAULT 'other'::text,
    notes text,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    item_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: businesses; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.businesses (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text NOT NULL,
    description text,
    owner_id uuid NOT NULL,
    archived_at timestamp without time zone,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    is_public_searchable boolean DEFAULT false NOT NULL,
    public_description text,
    city text,
    country text,
    address text,
    is_active boolean DEFAULT true NOT NULL
);


--
-- Name: businesses_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.businesses_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL
);


--
-- Name: chat_messages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.chat_messages (
    id uuid NOT NULL,
    content text NOT NULL,
    room_id uuid NOT NULL,
    sender_id uuid NOT NULL,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    sender_user_id uuid,
    sender_client_id uuid
);


--
-- Name: chat_participants; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.chat_participants (
    id uuid NOT NULL,
    room_id uuid NOT NULL,
    user_id uuid,
    client_id uuid,
    role public.citext DEFAULT 'member'::public.citext NOT NULL,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: chat_rooms; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.chat_rooms (
    id uuid NOT NULL,
    name character varying(255) NOT NULL,
    description text,
    room_type character varying(255) DEFAULT 'general'::character varying NOT NULL,
    is_active boolean DEFAULT true NOT NULL,
    business_id uuid NOT NULL,
    created_by_id uuid NOT NULL,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL
);


--
-- Name: clients; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.clients (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text NOT NULL,
    email public.citext,
    phone text,
    is_registered boolean DEFAULT false NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    archived_at timestamp without time zone,
    email_verified boolean DEFAULT false NOT NULL,
    verification_token character varying(255),
    business_id uuid NOT NULL,
    CONSTRAINT email_required_for_registered CHECK (((NOT is_registered) OR ((email IS NOT NULL) AND (email OPERATOR(public.<>) ''::public.citext)))),
    CONSTRAINT token_required_for_unverified CHECK ((email_verified OR (verification_token IS NOT NULL) OR (email IS NULL)))
);


--
-- Name: clients_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.clients_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: consent_records; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.consent_records (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    user_id uuid NOT NULL,
    business_id uuid,
    purpose text NOT NULL,
    consent_given boolean NOT NULL,
    consent_date timestamp(0) without time zone NOT NULL,
    withdrawal_date timestamp(0) without time zone,
    consent_version text NOT NULL,
    ip_address text,
    user_agent text,
    consent_method text DEFAULT 'web_form'::text NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: consent_records_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.consent_records_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: employee_permissions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.employee_permissions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    notes text,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    employee_id uuid NOT NULL,
    permission_id uuid NOT NULL,
    granted_by_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: employee_permissions_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.employee_permissions_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: employees; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.employees (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    email public.citext NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL,
    phone text,
    role text DEFAULT 'staff'::text,
    is_active boolean DEFAULT true NOT NULL,
    employee_number text,
    hire_date date,
    last_login_at timestamp(0) without time zone,
    notes text,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    business_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: employees_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.employees_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: item_holds; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.item_holds (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    reserved_from timestamp(0) without time zone NOT NULL,
    reserved_until timestamp(0) without time zone NOT NULL,
    expires_at timestamp(0) without time zone NOT NULL,
    hold_duration_minutes bigint DEFAULT 15 NOT NULL,
    is_active boolean DEFAULT true NOT NULL,
    released_at timestamp(0) without time zone,
    notes text,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    item_id uuid NOT NULL,
    client_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: item_positions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.item_positions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    grid_row bigint,
    grid_column bigint,
    x_coordinate numeric,
    y_coordinate numeric,
    width numeric,
    height numeric,
    rotation_degrees numeric DEFAULT '0'::numeric,
    z_index bigint DEFAULT 0,
    is_visible boolean DEFAULT true NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    item_id uuid NOT NULL,
    layout_id uuid NOT NULL,
    archived_at timestamp without time zone,
    business_id uuid NOT NULL
);


--
-- Name: item_schedules; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.item_schedules (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    day_of_week bigint NOT NULL,
    start_time time(0) without time zone NOT NULL,
    end_time time(0) without time zone NOT NULL,
    is_available boolean DEFAULT true NOT NULL,
    notes text,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    item_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: item_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.item_types (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text NOT NULL,
    description text,
    color text,
    icon text,
    is_active boolean DEFAULT true NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    business_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: item_types_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.item_types_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: items; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.items (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name character varying(255) NOT NULL,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    section_id uuid,
    description text,
    is_always_available boolean DEFAULT false NOT NULL,
    capacity bigint DEFAULT 1 NOT NULL,
    minimum_duration_minutes bigint,
    maximum_duration_minutes bigint,
    is_active boolean DEFAULT true,
    archived_at timestamp without time zone,
    item_type_id uuid,
    business_id uuid NOT NULL,
    is_public_searchable boolean DEFAULT false NOT NULL,
    public_description text
);


--
-- Name: items_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.items_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: layouts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.layouts (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text NOT NULL,
    layout_type text DEFAULT 'grid'::text,
    grid_rows bigint,
    grid_columns bigint,
    width numeric,
    height numeric,
    background_color text,
    background_image_url text,
    is_active boolean DEFAULT true NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    plot_id uuid NOT NULL,
    archived_at timestamp without time zone,
    business_id uuid NOT NULL
);


--
-- Name: layouts_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.layouts_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: payments; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payments (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    status text DEFAULT 'pending'::text,
    amount_due numeric NOT NULL,
    amount_paid numeric,
    currency text DEFAULT 'USD'::text NOT NULL,
    payment_method text DEFAULT 'cash'::text,
    payment_date timestamp(0) without time zone,
    due_date date,
    transaction_reference text,
    refund_amount numeric,
    refund_date timestamp(0) without time zone,
    refund_reason text,
    notes text,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    reservation_id uuid NOT NULL,
    pricing_id uuid,
    archived_at timestamp without time zone,
    business_id uuid NOT NULL
);


--
-- Name: payments_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payments_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: permissions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.permissions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text NOT NULL,
    description text NOT NULL,
    category text NOT NULL,
    is_assignable boolean DEFAULT true NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: permissions_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.permissions_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: plots; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.plots (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text NOT NULL,
    description text,
    address text,
    total_area numeric,
    area_unit text DEFAULT 'sqft'::text,
    coordinates jsonb,
    is_active boolean DEFAULT true NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    business_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: plots_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.plots_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: pricing; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pricing (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text,
    description text,
    pricing_type text DEFAULT 'base'::text,
    price_per_day numeric NOT NULL,
    currency text DEFAULT 'USD'::text NOT NULL,
    effective_from date,
    effective_until date,
    is_active boolean DEFAULT true NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    business_id uuid NOT NULL,
    item_type_id uuid NOT NULL,
    archived_at timestamp without time zone,
    weekday_price numeric,
    weekend_price numeric,
    has_day_type_pricing boolean DEFAULT false NOT NULL
);


--
-- Name: pricing_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pricing_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: recurring_reservation_instances; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.recurring_reservation_instances (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    scheduled_date date NOT NULL,
    sequence_number bigint NOT NULL,
    status text DEFAULT 'pending'::text,
    notes text,
    skip_reason text,
    error_message text,
    created_at timestamp(0) without time zone,
    failed_at timestamp(0) without time zone,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    recurring_reservation_id uuid NOT NULL,
    reservation_id uuid,
    archived_at timestamp without time zone
);


--
-- Name: recurring_reservation_instances_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.recurring_reservation_instances_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: recurring_reservations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.recurring_reservations (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title text,
    start_date date NOT NULL,
    start_time time(0) without time zone NOT NULL,
    end_time time(0) without time zone NOT NULL,
    consecutive_days bigint NOT NULL,
    pattern_type text DEFAULT 'consecutive_days'::text,
    status text DEFAULT 'pending'::text,
    notes text,
    instances_generated boolean DEFAULT false NOT NULL,
    generated_at timestamp(0) without time zone,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    client_id uuid NOT NULL,
    item_id uuid NOT NULL,
    employee_id uuid NOT NULL,
    archived_at timestamp without time zone
);


--
-- Name: recurring_reservations_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.recurring_reservations_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: reservations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.reservations (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    reserved_from timestamp(0) without time zone NOT NULL,
    reserved_until timestamp(0) without time zone NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    notes text,
    client_id uuid NOT NULL,
    item_id uuid NOT NULL,
    inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    employee_id uuid,
    archived_at timestamp without time zone,
    is_paid boolean DEFAULT false NOT NULL,
    total_amount numeric,
    business_id uuid NOT NULL,
    hold_expires_at timestamp(0) without time zone,
    is_provisional boolean DEFAULT false NOT NULL,
    number_of_days bigint,
    daily_rate numeric,
    multi_day_discount numeric,
    CONSTRAINT reservations_valid_time_range CHECK ((reserved_from < reserved_until))
);


--
-- Name: reservations_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.reservations_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_action_name text NOT NULL,
    version_action_inputs jsonb NOT NULL,
    version_resource_identifier text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version bigint NOT NULL,
    inserted_at timestamp(0) without time zone
);


--
-- Name: sections; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sections (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name character varying(255) NOT NULL,
    description text,
    plot_id uuid NOT NULL,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    archived_at timestamp without time zone,
    business_id uuid NOT NULL
);


--
-- Name: sections_versions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sections_versions (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_action_type text NOT NULL,
    version_source_id uuid NOT NULL,
    changes jsonb,
    version_inserted_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL,
    version_updated_at timestamp without time zone DEFAULT (now() AT TIME ZONE 'utc'::text) NOT NULL
);


--
-- Name: user_tokens; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_tokens (
    id uuid NOT NULL,
    subject character varying(255) NOT NULL,
    token text NOT NULL,
    purpose character varying(255) NOT NULL,
    expires_at timestamp(0) without time zone NOT NULL,
    extra_data jsonb,
    inserted_at timestamp without time zone NOT NULL
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id uuid NOT NULL,
    email public.citext NOT NULL,
    hashed_password character varying(255) NOT NULL,
    name character varying(255),
    role character varying(255) DEFAULT 'user'::character varying NOT NULL,
    inserted_at timestamp without time zone NOT NULL,
    updated_at timestamp without time zone NOT NULL,
    archived_at timestamp without time zone,
    CONSTRAINT users_role_check CHECK (((role)::text = ANY ((ARRAY['user'::character varying, 'admin'::character varying, 'superadmin'::character varying])::text[])))
);


--
-- Name: availability_exceptions availability_exceptions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.availability_exceptions
    ADD CONSTRAINT availability_exceptions_pkey PRIMARY KEY (id);


--
-- Name: businesses businesses_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.businesses
    ADD CONSTRAINT businesses_pkey PRIMARY KEY (id);


--
-- Name: businesses_versions businesses_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.businesses_versions
    ADD CONSTRAINT businesses_versions_pkey PRIMARY KEY (id);


--
-- Name: chat_messages chat_messages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_pkey PRIMARY KEY (id);


--
-- Name: chat_participants chat_participants_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_participants
    ADD CONSTRAINT chat_participants_pkey PRIMARY KEY (id);


--
-- Name: chat_rooms chat_rooms_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_rooms
    ADD CONSTRAINT chat_rooms_pkey PRIMARY KEY (id);


--
-- Name: clients clients_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.clients
    ADD CONSTRAINT clients_pkey PRIMARY KEY (id);


--
-- Name: clients_versions clients_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.clients_versions
    ADD CONSTRAINT clients_versions_pkey PRIMARY KEY (id);


--
-- Name: consent_records consent_records_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.consent_records
    ADD CONSTRAINT consent_records_pkey PRIMARY KEY (id);


--
-- Name: consent_records_versions consent_records_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.consent_records_versions
    ADD CONSTRAINT consent_records_versions_pkey PRIMARY KEY (id);


--
-- Name: employee_permissions employee_permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employee_permissions
    ADD CONSTRAINT employee_permissions_pkey PRIMARY KEY (id);


--
-- Name: employee_permissions_versions employee_permissions_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employee_permissions_versions
    ADD CONSTRAINT employee_permissions_versions_pkey PRIMARY KEY (id);


--
-- Name: employees employees_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employees
    ADD CONSTRAINT employees_pkey PRIMARY KEY (id);


--
-- Name: employees_versions employees_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employees_versions
    ADD CONSTRAINT employees_versions_pkey PRIMARY KEY (id);


--
-- Name: item_holds item_holds_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_holds
    ADD CONSTRAINT item_holds_pkey PRIMARY KEY (id);


--
-- Name: item_positions item_positions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_positions
    ADD CONSTRAINT item_positions_pkey PRIMARY KEY (id);


--
-- Name: item_schedules item_schedules_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_schedules
    ADD CONSTRAINT item_schedules_pkey PRIMARY KEY (id);


--
-- Name: item_types item_types_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_types
    ADD CONSTRAINT item_types_pkey PRIMARY KEY (id);


--
-- Name: item_types_versions item_types_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_types_versions
    ADD CONSTRAINT item_types_versions_pkey PRIMARY KEY (id);


--
-- Name: items items_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- Name: items_versions items_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.items_versions
    ADD CONSTRAINT items_versions_pkey PRIMARY KEY (id);


--
-- Name: layouts layouts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.layouts
    ADD CONSTRAINT layouts_pkey PRIMARY KEY (id);


--
-- Name: layouts_versions layouts_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.layouts_versions
    ADD CONSTRAINT layouts_versions_pkey PRIMARY KEY (id);


--
-- Name: payments payments_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payments
    ADD CONSTRAINT payments_pkey PRIMARY KEY (id);


--
-- Name: payments_versions payments_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payments_versions
    ADD CONSTRAINT payments_versions_pkey PRIMARY KEY (id);


--
-- Name: permissions permissions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.permissions
    ADD CONSTRAINT permissions_pkey PRIMARY KEY (id);


--
-- Name: permissions_versions permissions_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.permissions_versions
    ADD CONSTRAINT permissions_versions_pkey PRIMARY KEY (id);


--
-- Name: plots plots_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.plots
    ADD CONSTRAINT plots_pkey PRIMARY KEY (id);


--
-- Name: plots_versions plots_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.plots_versions
    ADD CONSTRAINT plots_versions_pkey PRIMARY KEY (id);


--
-- Name: pricing pricing_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pricing
    ADD CONSTRAINT pricing_pkey PRIMARY KEY (id);


--
-- Name: pricing_versions pricing_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pricing_versions
    ADD CONSTRAINT pricing_versions_pkey PRIMARY KEY (id);


--
-- Name: recurring_reservation_instances recurring_reservation_instances_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservation_instances
    ADD CONSTRAINT recurring_reservation_instances_pkey PRIMARY KEY (id);


--
-- Name: recurring_reservation_instances_versions recurring_reservation_instances_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservation_instances_versions
    ADD CONSTRAINT recurring_reservation_instances_versions_pkey PRIMARY KEY (id);


--
-- Name: recurring_reservations recurring_reservations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservations
    ADD CONSTRAINT recurring_reservations_pkey PRIMARY KEY (id);


--
-- Name: recurring_reservations_versions recurring_reservations_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservations_versions
    ADD CONSTRAINT recurring_reservations_versions_pkey PRIMARY KEY (id);


--
-- Name: reservations reservations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reservations
    ADD CONSTRAINT reservations_pkey PRIMARY KEY (id);


--
-- Name: reservations_versions reservations_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reservations_versions
    ADD CONSTRAINT reservations_versions_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: sections sections_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sections
    ADD CONSTRAINT sections_pkey PRIMARY KEY (id);


--
-- Name: sections_versions sections_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sections_versions
    ADD CONSTRAINT sections_versions_pkey PRIMARY KEY (id);


--
-- Name: user_tokens user_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_tokens
    ADD CONSTRAINT user_tokens_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: availability_exceptions_unique_item_date_time_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX availability_exceptions_unique_item_date_time_index ON public.availability_exceptions USING btree (item_id, date, start_time, end_time);


--
-- Name: businesses_name_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX businesses_name_index ON public.businesses USING btree (name);


--
-- Name: businesses_owner_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX businesses_owner_id_index ON public.businesses USING btree (owner_id);


--
-- Name: chat_messages_inserted_at_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_messages_inserted_at_index ON public.chat_messages USING btree (inserted_at);


--
-- Name: chat_messages_room_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_messages_room_id_index ON public.chat_messages USING btree (room_id);


--
-- Name: chat_messages_room_id_inserted_at_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_messages_room_id_inserted_at_index ON public.chat_messages USING btree (room_id, inserted_at);


--
-- Name: chat_messages_sender_client_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_messages_sender_client_id_index ON public.chat_messages USING btree (sender_client_id);


--
-- Name: chat_messages_sender_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_messages_sender_id_index ON public.chat_messages USING btree (sender_id);


--
-- Name: chat_messages_sender_user_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_messages_sender_user_id_index ON public.chat_messages USING btree (sender_user_id);


--
-- Name: chat_participants_client_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_participants_client_id_index ON public.chat_participants USING btree (client_id);


--
-- Name: chat_participants_room_id_client_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX chat_participants_room_id_client_id_index ON public.chat_participants USING btree (room_id, client_id) WHERE (client_id IS NOT NULL);


--
-- Name: chat_participants_room_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_participants_room_id_index ON public.chat_participants USING btree (room_id);


--
-- Name: chat_participants_room_id_user_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX chat_participants_room_id_user_id_index ON public.chat_participants USING btree (room_id, user_id) WHERE (user_id IS NOT NULL);


--
-- Name: chat_participants_user_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_participants_user_id_index ON public.chat_participants USING btree (user_id);


--
-- Name: chat_rooms_business_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_rooms_business_id_index ON public.chat_rooms USING btree (business_id);


--
-- Name: chat_rooms_created_by_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_rooms_created_by_id_index ON public.chat_rooms USING btree (created_by_id);


--
-- Name: chat_rooms_is_active_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_rooms_is_active_index ON public.chat_rooms USING btree (is_active);


--
-- Name: chat_rooms_room_type_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX chat_rooms_room_type_index ON public.chat_rooms USING btree (room_type);


--
-- Name: clients_business_id_email_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX clients_business_id_email_index ON public.clients USING btree (business_id, email);


--
-- Name: clients_email_verified_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX clients_email_verified_index ON public.clients USING btree (email_verified);


--
-- Name: clients_is_registered_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX clients_is_registered_index ON public.clients USING btree (is_registered);


--
-- Name: clients_unique_email_per_business_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX clients_unique_email_per_business_index ON public.clients USING btree (business_id, email);


--
-- Name: clients_verification_token_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX clients_verification_token_index ON public.clients USING btree (verification_token);


--
-- Name: consent_records_unique_user_purpose_date_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX consent_records_unique_user_purpose_date_index ON public.consent_records USING btree (user_id, purpose, consent_date);


--
-- Name: employee_permissions_unique_employee_permission_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX employee_permissions_unique_employee_permission_index ON public.employee_permissions USING btree (employee_id, permission_id);


--
-- Name: employees_business_id_is_active_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX employees_business_id_is_active_index ON public.employees USING btree (business_id, is_active);


--
-- Name: employees_unique_email_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX employees_unique_email_index ON public.employees USING btree (email);


--
-- Name: employees_unique_employee_number_per_business_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX employees_unique_employee_number_per_business_index ON public.employees USING btree (business_id, employee_number) WHERE (employee_number IS NOT NULL);


--
-- Name: item_holds_client_id_is_active_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX item_holds_client_id_is_active_index ON public.item_holds USING btree (client_id, is_active);


--
-- Name: item_holds_expires_at_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX item_holds_expires_at_index ON public.item_holds USING btree (expires_at);


--
-- Name: item_holds_item_id_is_active_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX item_holds_item_id_is_active_index ON public.item_holds USING btree (item_id, is_active);


--
-- Name: item_holds_unique_active_hold_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX item_holds_unique_active_hold_index ON public.item_holds USING btree (item_id, reserved_from, reserved_until, is_active);


--
-- Name: item_positions_business_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX item_positions_business_id_index ON public.item_positions USING btree (business_id);


--
-- Name: item_positions_layout_id_grid_row_grid_column_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX item_positions_layout_id_grid_row_grid_column_index ON public.item_positions USING btree (layout_id, grid_row, grid_column);


--
-- Name: item_positions_unique_grid_position_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX item_positions_unique_grid_position_index ON public.item_positions USING btree (layout_id, grid_row, grid_column);


--
-- Name: item_positions_unique_item_per_layout_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX item_positions_unique_item_per_layout_index ON public.item_positions USING btree (item_id, layout_id);


--
-- Name: item_schedules_unique_item_day_time_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX item_schedules_unique_item_day_time_index ON public.item_schedules USING btree (item_id, day_of_week, start_time, end_time);


--
-- Name: item_types_unique_name_per_business_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX item_types_unique_name_per_business_index ON public.item_types USING btree (name, business_id);


--
-- Name: items_business_id_is_active_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX items_business_id_is_active_index ON public.items USING btree (business_id, is_active);


--
-- Name: items_business_id_item_type_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX items_business_id_item_type_id_index ON public.items USING btree (business_id, item_type_id);


--
-- Name: items_name_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX items_name_index ON public.items USING btree (name);


--
-- Name: items_section_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX items_section_id_index ON public.items USING btree (section_id);


--
-- Name: items_unique_name_per_business_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX items_unique_name_per_business_index ON public.items USING btree (name, business_id);


--
-- Name: layouts_business_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX layouts_business_id_index ON public.layouts USING btree (business_id);


--
-- Name: layouts_plot_id_name_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX layouts_plot_id_name_index ON public.layouts USING btree (plot_id, name);


--
-- Name: layouts_unique_name_per_plot_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX layouts_unique_name_per_plot_index ON public.layouts USING btree (name, plot_id);


--
-- Name: payments_business_id_payment_date_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payments_business_id_payment_date_index ON public.payments USING btree (business_id, payment_date);


--
-- Name: payments_business_id_payment_method_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX payments_business_id_payment_method_index ON public.payments USING btree (business_id, payment_method);


--
-- Name: permissions_unique_name_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX permissions_unique_name_index ON public.permissions USING btree (name);


--
-- Name: plots_business_id_name_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX plots_business_id_name_index ON public.plots USING btree (business_id, name);


--
-- Name: plots_unique_name_per_business_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX plots_unique_name_per_business_index ON public.plots USING btree (name, business_id);


--
-- Name: pricing_business_id_effective_from_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX pricing_business_id_effective_from_index ON public.pricing USING btree (business_id, effective_from);


--
-- Name: pricing_business_id_item_type_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX pricing_business_id_item_type_id_index ON public.pricing USING btree (business_id, item_type_id);


--
-- Name: pricing_unique_pricing_rule_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX pricing_unique_pricing_rule_index ON public.pricing USING btree (business_id, item_type_id, pricing_type, effective_from, effective_until);


--
-- Name: recurring_reservation_instances_unique_recurring_date_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX recurring_reservation_instances_unique_recurring_date_index ON public.recurring_reservation_instances USING btree (recurring_reservation_id, scheduled_date);


--
-- Name: recurring_reservation_instances_unique_recurring_sequence_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX recurring_reservation_instances_unique_recurring_sequence_index ON public.recurring_reservation_instances USING btree (recurring_reservation_id, sequence_number);


--
-- Name: reservations_business_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_business_id_index ON public.reservations USING btree (business_id);


--
-- Name: reservations_business_id_reserved_from_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_business_id_reserved_from_index ON public.reservations USING btree (business_id, reserved_from);


--
-- Name: reservations_business_id_reserved_from_reserved_until_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_business_id_reserved_from_reserved_until_index ON public.reservations USING btree (business_id, reserved_from, reserved_until);


--
-- Name: reservations_business_id_status_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_business_id_status_index ON public.reservations USING btree (business_id, status);


--
-- Name: reservations_client_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_client_id_index ON public.reservations USING btree (client_id);


--
-- Name: reservations_inserted_at_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_inserted_at_index ON public.reservations USING btree (inserted_at);


--
-- Name: reservations_item_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_item_id_index ON public.reservations USING btree (item_id);


--
-- Name: reservations_item_time_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_item_time_index ON public.reservations USING btree (item_id, reserved_from, reserved_until) WHERE (status = ANY (ARRAY['pending'::text, 'confirmed'::text]));


--
-- Name: reservations_reserved_from_reserved_until_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_reserved_from_reserved_until_index ON public.reservations USING btree (reserved_from, reserved_until);


--
-- Name: reservations_unique_item_time_slot_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX reservations_unique_item_time_slot_index ON public.reservations USING btree (item_id, reserved_from, reserved_until);


--
-- Name: reservations_updated_at_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX reservations_updated_at_index ON public.reservations USING btree (updated_at);


--
-- Name: sections_business_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX sections_business_id_index ON public.sections USING btree (plot_id);


--
-- Name: sections_name_business_id_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX sections_name_business_id_index ON public.sections USING btree (name, plot_id);


--
-- Name: sections_plot_id_name_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX sections_plot_id_name_index ON public.sections USING btree (plot_id, name);


--
-- Name: sections_unique_name_per_plot_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX sections_unique_name_per_plot_index ON public.sections USING btree (name, plot_id);


--
-- Name: unique_room_name_per_business; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX unique_room_name_per_business ON public.chat_rooms USING btree (business_id, name);


--
-- Name: user_tokens_subject_index; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX user_tokens_subject_index ON public.user_tokens USING btree (subject);


--
-- Name: user_tokens_token_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX user_tokens_token_index ON public.user_tokens USING btree (token);


--
-- Name: users_email_index; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX users_email_index ON public.users USING btree (email);


--
-- Name: availability_exceptions availability_exceptions_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.availability_exceptions
    ADD CONSTRAINT availability_exceptions_item_id_fkey FOREIGN KEY (item_id) REFERENCES public.items(id);


--
-- Name: businesses_versions businesses_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.businesses_versions
    ADD CONSTRAINT businesses_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.businesses(id);


--
-- Name: chat_messages chat_messages_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.chat_rooms(id) ON DELETE CASCADE;


--
-- Name: chat_messages chat_messages_sender_client_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_sender_client_id_fkey FOREIGN KEY (sender_client_id) REFERENCES public.clients(id) ON DELETE CASCADE;


--
-- Name: chat_messages chat_messages_sender_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_sender_id_fkey FOREIGN KEY (sender_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: chat_messages chat_messages_sender_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_sender_user_id_fkey FOREIGN KEY (sender_user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: chat_participants chat_participants_client_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_participants
    ADD CONSTRAINT chat_participants_client_id_fkey FOREIGN KEY (client_id) REFERENCES public.clients(id) ON DELETE CASCADE;


--
-- Name: chat_participants chat_participants_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_participants
    ADD CONSTRAINT chat_participants_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.chat_rooms(id) ON DELETE CASCADE;


--
-- Name: chat_participants chat_participants_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_participants
    ADD CONSTRAINT chat_participants_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: chat_rooms chat_rooms_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_rooms
    ADD CONSTRAINT chat_rooms_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id) ON DELETE CASCADE;


--
-- Name: chat_rooms chat_rooms_created_by_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_rooms
    ADD CONSTRAINT chat_rooms_created_by_id_fkey FOREIGN KEY (created_by_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: clients clients_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.clients
    ADD CONSTRAINT clients_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: clients_versions clients_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.clients_versions
    ADD CONSTRAINT clients_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.clients(id);


--
-- Name: consent_records consent_records_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.consent_records
    ADD CONSTRAINT consent_records_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: consent_records consent_records_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.consent_records
    ADD CONSTRAINT consent_records_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: consent_records_versions consent_records_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.consent_records_versions
    ADD CONSTRAINT consent_records_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.consent_records(id);


--
-- Name: employee_permissions employee_permissions_employee_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employee_permissions
    ADD CONSTRAINT employee_permissions_employee_id_fkey FOREIGN KEY (employee_id) REFERENCES public.employees(id);


--
-- Name: employee_permissions employee_permissions_granted_by_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employee_permissions
    ADD CONSTRAINT employee_permissions_granted_by_id_fkey FOREIGN KEY (granted_by_id) REFERENCES public.employees(id);


--
-- Name: employee_permissions employee_permissions_permission_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employee_permissions
    ADD CONSTRAINT employee_permissions_permission_id_fkey FOREIGN KEY (permission_id) REFERENCES public.permissions(id);


--
-- Name: employee_permissions_versions employee_permissions_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employee_permissions_versions
    ADD CONSTRAINT employee_permissions_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.employee_permissions(id);


--
-- Name: employees employees_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employees
    ADD CONSTRAINT employees_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: employees_versions employees_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.employees_versions
    ADD CONSTRAINT employees_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.employees(id);


--
-- Name: item_holds item_holds_client_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_holds
    ADD CONSTRAINT item_holds_client_id_fkey FOREIGN KEY (client_id) REFERENCES public.clients(id);


--
-- Name: item_holds item_holds_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_holds
    ADD CONSTRAINT item_holds_item_id_fkey FOREIGN KEY (item_id) REFERENCES public.items(id);


--
-- Name: item_positions item_positions_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_positions
    ADD CONSTRAINT item_positions_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: item_positions item_positions_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_positions
    ADD CONSTRAINT item_positions_item_id_fkey FOREIGN KEY (item_id) REFERENCES public.items(id);


--
-- Name: item_positions item_positions_layout_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_positions
    ADD CONSTRAINT item_positions_layout_id_fkey FOREIGN KEY (layout_id) REFERENCES public.layouts(id);


--
-- Name: item_schedules item_schedules_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_schedules
    ADD CONSTRAINT item_schedules_item_id_fkey FOREIGN KEY (item_id) REFERENCES public.items(id);


--
-- Name: item_types item_types_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_types
    ADD CONSTRAINT item_types_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: item_types_versions item_types_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.item_types_versions
    ADD CONSTRAINT item_types_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.item_types(id);


--
-- Name: items items_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT items_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: items items_item_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT items_item_type_id_fkey FOREIGN KEY (item_type_id) REFERENCES public.item_types(id);


--
-- Name: items items_section_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT items_section_id_fkey FOREIGN KEY (section_id) REFERENCES public.sections(id) ON DELETE SET NULL;


--
-- Name: items_versions items_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.items_versions
    ADD CONSTRAINT items_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.items(id);


--
-- Name: layouts layouts_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.layouts
    ADD CONSTRAINT layouts_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: layouts layouts_plot_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.layouts
    ADD CONSTRAINT layouts_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES public.plots(id);


--
-- Name: layouts_versions layouts_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.layouts_versions
    ADD CONSTRAINT layouts_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.layouts(id);


--
-- Name: payments payments_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payments
    ADD CONSTRAINT payments_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: payments payments_pricing_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payments
    ADD CONSTRAINT payments_pricing_id_fkey FOREIGN KEY (pricing_id) REFERENCES public.pricing(id);


--
-- Name: payments payments_reservation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payments
    ADD CONSTRAINT payments_reservation_id_fkey FOREIGN KEY (reservation_id) REFERENCES public.reservations(id);


--
-- Name: payments_versions payments_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payments_versions
    ADD CONSTRAINT payments_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.payments(id);


--
-- Name: permissions_versions permissions_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.permissions_versions
    ADD CONSTRAINT permissions_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.permissions(id);


--
-- Name: plots plots_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.plots
    ADD CONSTRAINT plots_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: plots_versions plots_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.plots_versions
    ADD CONSTRAINT plots_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.plots(id);


--
-- Name: pricing pricing_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pricing
    ADD CONSTRAINT pricing_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: pricing pricing_item_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pricing
    ADD CONSTRAINT pricing_item_type_id_fkey FOREIGN KEY (item_type_id) REFERENCES public.item_types(id);


--
-- Name: pricing_versions pricing_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pricing_versions
    ADD CONSTRAINT pricing_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.pricing(id);


--
-- Name: recurring_reservation_instances recurring_reservation_instances_recurring_reservation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservation_instances
    ADD CONSTRAINT recurring_reservation_instances_recurring_reservation_id_fkey FOREIGN KEY (recurring_reservation_id) REFERENCES public.recurring_reservations(id);


--
-- Name: recurring_reservation_instances recurring_reservation_instances_reservation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservation_instances
    ADD CONSTRAINT recurring_reservation_instances_reservation_id_fkey FOREIGN KEY (reservation_id) REFERENCES public.reservations(id);


--
-- Name: recurring_reservation_instances_versions recurring_reservation_instances_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservation_instances_versions
    ADD CONSTRAINT recurring_reservation_instances_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.recurring_reservation_instances(id);


--
-- Name: recurring_reservations recurring_reservations_client_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservations
    ADD CONSTRAINT recurring_reservations_client_id_fkey FOREIGN KEY (client_id) REFERENCES public.clients(id);


--
-- Name: recurring_reservations recurring_reservations_employee_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservations
    ADD CONSTRAINT recurring_reservations_employee_id_fkey FOREIGN KEY (employee_id) REFERENCES public.employees(id);


--
-- Name: recurring_reservations recurring_reservations_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservations
    ADD CONSTRAINT recurring_reservations_item_id_fkey FOREIGN KEY (item_id) REFERENCES public.items(id);


--
-- Name: recurring_reservations_versions recurring_reservations_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.recurring_reservations_versions
    ADD CONSTRAINT recurring_reservations_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.recurring_reservations(id);


--
-- Name: reservations reservations_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reservations
    ADD CONSTRAINT reservations_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: reservations reservations_client_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reservations
    ADD CONSTRAINT reservations_client_id_fkey FOREIGN KEY (client_id) REFERENCES public.clients(id) ON DELETE RESTRICT;


--
-- Name: reservations reservations_employee_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reservations
    ADD CONSTRAINT reservations_employee_id_fkey FOREIGN KEY (employee_id) REFERENCES public.employees(id);


--
-- Name: reservations reservations_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reservations
    ADD CONSTRAINT reservations_item_id_fkey FOREIGN KEY (item_id) REFERENCES public.items(id) ON DELETE RESTRICT;


--
-- Name: reservations_versions reservations_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reservations_versions
    ADD CONSTRAINT reservations_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.reservations(id);


--
-- Name: sections sections_business_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sections
    ADD CONSTRAINT sections_business_id_fkey FOREIGN KEY (business_id) REFERENCES public.businesses(id);


--
-- Name: sections sections_plot_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sections
    ADD CONSTRAINT sections_plot_id_fkey FOREIGN KEY (plot_id) REFERENCES public.plots(id);


--
-- Name: sections_versions sections_versions_version_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sections_versions
    ADD CONSTRAINT sections_versions_version_source_id_fkey FOREIGN KEY (version_source_id) REFERENCES public.sections(id);


--
-- PostgreSQL database dump complete
--

INSERT INTO public."schema_migrations" (version) VALUES (20240100999999);
INSERT INTO public."schema_migrations" (version) VALUES (20240101000000);
INSERT INTO public."schema_migrations" (version) VALUES (20240101000001);
INSERT INTO public."schema_migrations" (version) VALUES (20240101000002);
INSERT INTO public."schema_migrations" (version) VALUES (20240101000003);
INSERT INTO public."schema_migrations" (version) VALUES (20240101000004);
INSERT INTO public."schema_migrations" (version) VALUES (20240807000001);
INSERT INTO public."schema_migrations" (version) VALUES (20240807000002);
INSERT INTO public."schema_migrations" (version) VALUES (20250702232140);
INSERT INTO public."schema_migrations" (version) VALUES (20250702232225);
INSERT INTO public."schema_migrations" (version) VALUES (20250702233854);
INSERT INTO public."schema_migrations" (version) VALUES (20250703064819);
INSERT INTO public."schema_migrations" (version) VALUES (20250703070126);
INSERT INTO public."schema_migrations" (version) VALUES (20250703134328);
INSERT INTO public."schema_migrations" (version) VALUES (20250703152356);
INSERT INTO public."schema_migrations" (version) VALUES (20250703175411);
INSERT INTO public."schema_migrations" (version) VALUES (20250704181801);
INSERT INTO public."schema_migrations" (version) VALUES (20250705135251);
INSERT INTO public."schema_migrations" (version) VALUES (20250705142134);
INSERT INTO public."schema_migrations" (version) VALUES (20250705143000);
INSERT INTO public."schema_migrations" (version) VALUES (20250711200613);
INSERT INTO public."schema_migrations" (version) VALUES (20250711222649);
INSERT INTO public."schema_migrations" (version) VALUES (20250713131054);
INSERT INTO public."schema_migrations" (version) VALUES (20250713131055);
INSERT INTO public."schema_migrations" (version) VALUES (20250713131056);
INSERT INTO public."schema_migrations" (version) VALUES (20250713133232);
INSERT INTO public."schema_migrations" (version) VALUES (20250727222949);
INSERT INTO public."schema_migrations" (version) VALUES (20250727224917);
INSERT INTO public."schema_migrations" (version) VALUES (20250728130926);
INSERT INTO public."schema_migrations" (version) VALUES (20250809120000);
INSERT INTO public."schema_migrations" (version) VALUES (20250809121000);
INSERT INTO public."schema_migrations" (version) VALUES (20250811195500);
