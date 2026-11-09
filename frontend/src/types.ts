export interface Job {
  id: string;
  company: string;
  company_website: string;
  origin: string;
  title: string;
  location: string;
  description_text: string;
  description_snippet: string;
  url: string;
  source: string;
  posted_at: string | null; // ISO 8601, or null for unconfirmed (DOM-fallback) jobs
  matched_roles?: string[]; // keys into Meta.role_filter
  first_seen_at?: string; // when the crawler first saw this role (ISO 8601)
  location_kind?: "london" | "south_east" | "remote_uk";
  employment_type?: string;
  salary_text?: string | null; // stated GBP pay, e.g. "£120k–£150k"
  salary_max_gbp?: number | null;
}

export interface Meta {
  generated_at: string;
  window_hours: number;
  companies_checked: number;
  companies_with_matches: string[];
  companies_with_unconfirmed: string[];
  companies_dom_fallback: string[];
  companies_failed: string[];
  total_jobs_found: number;
  total_unconfirmed_found: number;
  role_filter?: Record<string, string> | null; // role key -> display label
  dropped_by_role_filter?: number;
  dropped_wrong_city?: number;
  dropped_stale_unconfirmed?: number;
  highlight_hours?: number;
  filters?: { locations: string[]; permanent_only: boolean; min_salary_gbp: number | null };
  dropped_not_permanent?: number;
  dropped_not_senior?: number;
  dropped_below_salary?: number;
}
